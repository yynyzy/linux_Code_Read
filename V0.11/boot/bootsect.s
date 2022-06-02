!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
/*
  开机时，主板上提前写死的固件程序 BIOS 会将硬盘中启动区的 512 字节的数据，原封不动复制到内存中的 0x7c00 这个位置，并跳转到那个位置进行执行。
  启动区:只要硬盘中的 0 盘 0 道 1 扇区的 512 个字节的最后两个字节分别是 0x55 和 0xaa，那么 BIOS 就会认为它是个启动区。
  Linux-0.11最开始代码，是用汇编语言写的 bootsect.s，位于 boot 文件夹下。通过编译，这个 bootsect.s 会被编译成二进制文件，存放在启动区的第一扇区。
*/ 
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

SETUPLEN = 4				! nr of setup-sectors
BOOTSEG  = 0x07c0			! original address of boot-sector
INITSEG  = 0x9000			! we move boot here - out of the way
SETUPSEG = 0x9020			! setup starts here
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536).
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc
ROOT_DEV = 0x306

entry start
start:
	mov	ax,#BOOTSEG  
	mov	ds,ax 
	/* BOOTSEG(0x07c0) 复制到 ax 寄存器里，再将 ax 寄存器里的值复制到 ds 寄存器里。 ds 寄存器里的值变成了 0x07c0。
 	ds 是一个 16 位的段寄存器，具体表示数据段寄存器，在内存寻址时充当段基址的作用。
 	ds 被赋值为了 0x07c0,由于 x86 为了让自己在 16 位这个实模式下能访问到 20 位的地址线这个历史因素,所以段基址的二进制形式要先左移四位(即16进制左移一位)。那 0x07c0 左移四位就是 0x7c00,那这就刚好和这段代码被 BIOS 加载到的内存地址 0x7c00 一样了。
 	BIOS 规定死了把操作系统代码加载到内存 0x7c00,里面的各种数据自然就全都被偏移了这么多，所以把数据段寄存器 ds 设置为这个值，方便了以后通过这种基址的方式访问内存里的数据。
	*/
	mov	ax,#INITSEG
	mov	es,ax
	mov	cx,#256
	/*
	将 es 寄存器的值变成 0x9000
	把 cx 寄存器的值变成 256
	*/
	sub	si,si   //sub 后面的两个寄存器一模一样，就相当于把这个寄存器里的值清零
	sub	di,di
	rep movw

	/*
	rep 表示重复执行后面的指令。movw 表示复制一个字(word 16位)。
	问题？
	1.重复执行多少次呢？是 cx 寄存器中的值，也就是 256 次。
	2.从哪复制到哪呢？是从 ds:si 处复制到 es:di 处。
	3.一次复制多少呢?刚刚说过了,复制一个字,16 位,也就是两个字节。
	！完整意思：
	将内存地址 0x7c00 处开始往后的 512 字节的数据，原封不动复制到 0x90000 处。(一次复制2个字节,复制256次,正好复制完512个字节)
	*/

	jmpi	go,INITSEG

	/*
	jmpi 是一个段间跳转指令，表示跳转到 0x9000:go 处执行(INITSEG 开头定义为 0x9000)
	=跳转到 0x90000 + go 这个内存地址处执行
	go 就是一个标签，最终编译成机器码的时候会被翻译成一个值，这个值就是 go 这个标签在文件内的偏移地址
	*/

go:	mov	ax,cs
	mov	ds,ax
	mov	es,ax
	mov	ss,ax
	mov	sp,#0xFF00		! arbitrary value >>512
/*
	cs 寄存器的值分别复制给 ds、es 和 ss 寄存器,把 0xFF00 给了 sp 寄存器。
	cs 寄存器表示代码段寄存器,CPU 当前正在执行的代码在内存中的位置，就是由 cs:ip 这组寄存器配合指向的,其中 cs 是基址,ip 是偏移地址。所以现在 cs 寄存器里的值就是 0x9000,ip 寄存器里的值是 go 这个标签的偏移地址。

	ds 为数据段寄存器，之前的代码在 0x7c00 处，现在代码已经被挪到了 0x90000 处，现在自然又改赋值为 0x9000 了

	ss 为栈段寄存器，后面要配合栈基址寄存器 sp 来表示此时的栈顶地址。而此时 sp 寄存器被赋值为了 0xFF00 了，所以目前的栈顶地址就是 ss:sp 所指向的地址 0x9FF00 处
*/

/*
拔高一下:操作系统在做的事情，就是给如何访问代码，如何访问数据，如何访问栈进行了一下内存的初步规划。其中访问代码和访问数据的规划方式就是设置了一个基址而已，访问栈就是把栈顶指针指向了一个远离代码位置的地方而已
*/


load_setup:
	mov	dx,#0x0000		! drive 0, head 0
	mov	cx,#0x0002		! sector 2, track 0
	mov	bx,#0x0200		! address = 512, in INITSEG
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors
	int	0x13			! read it

/*
 int 是汇编指令。int 0x13 表示发起 0x13 号中断，这条指令上面给 dx、cx、bx、ax 赋值都是作为这个中断程序的参数

这个中断发起后，CPU 会通过这个中断号，去寻找对应的中断处理程序的入口地址，并跳转过去执行，逻辑上就相当于执行了一个函数。而 0x13 号中断的处理程序是 BIOS 提前给我们写好的，是读取磁盘的相关功能的函数
*/
	jnc	ok_load_setup		! ok - continue
	mov	dx,#0x0000
	mov	ax,#0x0000		! reset the diskette
	int	0x13
	j	load_setup

ok_load_setup:

/*
上面代码将硬盘的第 2 个扇区开始，把数据加载到内存 0x90200 处，共加载 4 个扇区
如果复制成功，就跳转到 ok_load_setup 这个标签，如果失败，则会不断重复执行这段代码，也就是重试
*/




! Get disk drive parameters, specifically nr of sectors/track

	mov	dl,#0x00
	mov	ax,#0x0800		! AH=8 is get drive parameters
	int	0x13
	mov	ch,#0x00
	seg cs
	mov	sectors,cx
	mov	ax,#INITSEG
	mov	es,ax

! Print some inane message

	mov	ah,#0x03		! read cursor pos
	xor	bh,bh
	int	0x10
	
	mov	cx,#24
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10


	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000
	call	read_it
	call	kill_motor


	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG

/*
上面代码主要把从硬盘第 6 个扇区开始往后的 240 个扇区，加载到内存 0x10000 处
然后跳转指令 jmpi 0,SETUPSEG (SETUPSEG = 0x9020) 跳转到 0x90200 处，就是硬盘第二个扇区开始处的内容
*/


sread:	.word 1+SETUPLEN	! sectors read of current track
head:	.word 0			! current head
track:	.word 0			! current track

read_it:
	mov ax,es
	test ax,#0x0fff
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread
	mov cx,ax
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ax,#0x1000
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	mov dl,#0
	and dx,#0x0100
	mov ah,#2
	int 0x13
	jc bad_rt
	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:
