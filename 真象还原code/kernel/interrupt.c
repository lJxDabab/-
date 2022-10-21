#include "interrupt.h"
#include "stdint.h"
#include "global.h"
#include "io.h"
#include "print.h"

#define PIC_M_CTRL 0x20	        // 这里用的可编程中断控制器是8259A,主片的控制端口是0x20
#define PIC_M_DATA 0x21	        // 主片的数据端口是0x21
#define PIC_S_CTRL 0xa0	        // 从片的控制端口是0xa0
#define PIC_S_DATA 0xa1	        // 从片的数据端口是0xa1

#define IDT_DESC_CNT 0x81	    // 目前总共支持的中断数

#define EFLAGS_IF   0x00000200       // eflags寄存器中的if位为1
#define GET_EFLAGS(EFLAG_VAR) asm volatile("pushfl; popl %0" : "=g" (EFLAG_VAR))

extern uint32_t syscall_handler(void);

/*中断门描述符结构体*/
struct gate_desc {
   uint16_t    func_offset_low_word;//中断处理程序在目标代码段的偏移低 16 位
   uint16_t    selector;        //中断选择子
   uint8_t     dcount;          //此项为双字计数字段，是门描述符中的第4字节。此项固定值，不用考虑[未使用]
   uint8_t     attribute;       //高32位的8-15位
   uint16_t    func_offset_high_word;//中断处理程序在目标代码段的偏移高 16 位
};

// 静态函数声明,非必须
static void make_idt_desc(struct gate_desc* p_gdesc, uint8_t attr, intr_handler function);
static struct gate_desc idt[IDT_DESC_CNT];   // idt是中断描述符表,本质上就是个中断门描述符数组

char* intr_name[IDT_DESC_CNT];// 用于保存异常的名字
intr_handler idt_table[IDT_DESC_CNT]; // 定义中断处理程序数组.在kernel.S中定义的intrXXentry只是中断处理程序的入口,最终调用的是ide_table中的处理程序
extern intr_handler intr_entry_table[IDT_DESC_CNT];	    // 声明引用定义在kernel.S中的中断处理函数入口数组

/* 初始化可编程中断控制器8259A */
static void pic_init(void){

    /* 初始化主片 */
    outb (PIC_M_CTRL, 0x11);//ICW1:边沿触发，级联模式，需要ICW4
    outb (PIC_M_DATA, 0X20);//ICW2:0-31中断号被占用、保留，我们的中断从32开始.IR[0-7] 为 0x20 ~ 0x27.
    outb (PIC_M_DATA, 0x04);//ICW3: IR2接从片
    outb (PIC_M_DATA, 0x11);//ICW4: 8086模式, 正常EOI(手动结束)

    /* 初始化从片 */
    outb (PIC_S_CTRL, 0x11);//ICW1:边沿触发，级联模式，需要ICW4
    outb (PIC_S_DATA, 0x28);//ICW2: 起始中断向量号为0x28.IR[8-15] 为 0x28 ~ 0x2F.
    outb (PIC_S_DATA, 0x02);//主片的ID2连接本重片

    /* IRQ2用于级联从片,必须打开,否则无法响应从片上的中断
    主片上打开的中断有IRQ0的时钟,IRQ1的键盘和级联从片的IRQ2,其它全部关闭 */
    outb (PIC_M_DATA, 0xf8);

    /* 打开从片上的IRQ14,此引脚接收硬盘控制器的中断 */
    outb (PIC_S_DATA, 0xbf);

    put_str("   pic_init done\n");
}

/* 创建中断门描述符 */
static void make_idt_desc(struct gate_desc* p_gdesc, uint8_t attr, intr_handler function){
    p_gdesc->func_offset_low_word = (uint32_t)function & 0x0000ffff;
    p_gdesc->func_offset_high_word = ((uint32_t)function & 0xffff0000) >> 16;
    p_gdesc->dcount = 0;
    p_gdesc->attribute = attr;
    p_gdesc->selector = SELECTOR_K_CODE;//内核代码段
}

static void idt_desc_init(void){
    int i,last_index = IDT_DESC_CNT -1;
    for ( i = 0; i < IDT_DESC_CNT; i++)
    {
        make_idt_desc(&idt[i], IDT_DESC_ATTR_DPL0, intr_entry_table[i]);
    }
    /* 单独处理系统调用,系统调用对应的中断门dpl为3,
    * 中断处理程序为单独的syscall_handler */
    make_idt_desc(&idt[last_index], IDT_DESC_ATTR_DPL3,syscall_handler);
    put_str("   idt_desc_init done\n");
}

/* 通用的中断处理函数,一般用在异常出现时的处理 
*vec_nr 为 中断号
*/
static void general_intr_handler(uint8_t vec_nr){
    if (vec_nr == 0x27 || vec_nr == 0x2f) {	// 0x2f是从片8259A上的最后一个irq引脚，保留
      return;		//IRQ7和IRQ15会产生伪中断(spurious interrupt),无须处理。0x20+7 = 0x27
    }
    set_cursor(0);
    int cursor_pos = 0;
    while (cursor_pos<320)
    {
        put_char(' ');
        cursor_pos++;
    }
    set_cursor(0);
    put_str("!!!!!!!      excetion message begin  !!!!!!!!\n");
    set_cursor(88);	// 从第2行第8个字符开始打印
    put_str(intr_name[vec_nr]);
    if (vec_nr == 14) {	  // 若为Pagefault,将缺失的地址打印出来并悬停
        int page_fault_vaddr = 0; 
        asm("movl %%cr2, %0" : "=r" (page_fault_vaddr));
        put_str("\npage fault addr is ");
        put_int(page_fault_vaddr);
        }
    put_str("\n!!!!!!!      excetion message end    !!!!!!!!\n");
    while(1);
}

/* 完成一般中断处理函数注册及异常名称注册 */
static void exception_init(void){
    int i;
    for (i = 0; i < IDT_DESC_CNT; i++) {

/* idt_table数组中的函数是在进入中断后根据中断向量号调用的,
 * 见kernel/kernel.S的call [idt_table + %1*4] */
        idt_table[i] = general_intr_handler;		    // 默认为general_intr_handler。
							    // 以后会由register_handler来注册具体处理函数。
        intr_name[i] = "unknown";				    // 先统一赋值为unknown 
    }
    intr_name[0] = "#DE Divide Error";
    intr_name[1] = "#DB Debug Exception";
    intr_name[2] = "NMI Interrupt";
    intr_name[3] = "#BP Breakpoint Exception";
    intr_name[4] = "#OF Overflow Exception";
    intr_name[5] = "#BR BOUND Range Exceeded Exception";
    intr_name[6] = "#UD Invalid Opcode Exception";
    intr_name[7] = "#NM Device Not Available Exception";
    intr_name[8] = "#DF Double Fault Exception";
    intr_name[9] = "Coprocessor Segment Overrun";
    intr_name[10] = "#TS Invalid TSS Exception";
    intr_name[11] = "#NP Segment Not Present";
    intr_name[12] = "#SS Stack Fault Exception";
    intr_name[13] = "#GP General Protection Exception";
    intr_name[14] = "#PF Page-Fault Exception";
    // intr_name[15] 第15项是intel保留项，未使用
    intr_name[16] = "#MF x87 FPU Floating-Point Error";
    intr_name[17] = "#AC Alignment Check Exception";
    intr_name[18] = "#MC Machine-Check Exception";
    intr_name[19] = "#XF SIMD Floating-Point Exception";

}

/*完成有关中断的所有初始化工作*/
void idt_init(){
    put_str("idt_init start\n");
    idt_desc_init();// 初始化中断描述符表
    exception_init();// 异常名初始化并注册通常的中断处理函数
    pic_init();// 初始化8259A
    uint64_t idt_operand = ((sizeof(idt) - 1) | ( (uint64_t) ((uint32_t) idt)) << 16);//要写入IDTR的数据
    asm volatile("lidt %0" : : "m" (idt_operand));
    put_str("idt_init done\n");
}

/* 获取当前中断状态 */
enum intr_status intr_get_status(void){
    uint32_t eflags = 0;
    GET_EFLAGS(eflags);
    return (eflags & EFLAGS_IF) ? INTR_ON : INTR_OFF ;
}

/* 将中断状态设置为status */
enum intr_status intr_set_status (enum intr_status status){
    return (status & INTR_ON) ? intr_enable() : intr_disable();
}

/* 开中断并返回开中断前的状态*/
enum intr_status intr_enable (void){
    enum intr_status oldstatus;
    if (INTR_ON == intr_get_status())
    {
        oldstatus = INTR_ON;
        return oldstatus;
    }else{
        oldstatus = INTR_OFF;
        asm volatile("sti");// 开中断,sti指令将IF位置1
        return oldstatus;
    }
}

/* 关中断,并且返回关中断前的状态 */
enum intr_status intr_disable (void){
    enum intr_status oldstatus;
    if (INTR_ON == intr_get_status())
    {
        oldstatus = INTR_ON;
        asm volatile("cli":::"memory");// 关中断,cli指令将IF位置0
        return oldstatus;
    }else{
        oldstatus = INTR_OFF;
        return oldstatus;
    }
}

/* 在中断处理程序数组第vector_no个元素中注册安装中断处理程序function */
void register_handler(uint8_t vector_no, intr_handler function) {
/* idt_table数组中的函数是在进入中断后根据中断向量号调用的,
 * 见kernel/kernel.S的call [idt_table + %1*4] */
   idt_table[vector_no] = function; 
}