	.file	1 "custom_04_driver.c"
	.section .mdebug.abi32
	.previous
	.nan	legacy
	.module	fp=32
	.module	oddspreg
	.abicalls
	.text
	.align	2
	.globl	main
	.set	nomips16
	.set	nomicromips
	.ent	main
	.type	main, @function
main:
	.frame	$fp,32,$31		# vars= 0, regs= 2/0, args= 16, gp= 8
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.cpload	$25
	.set	reorder
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$fp,24($sp)
	move	$fp,$sp
	.cprestore	16
	li	$4,7			# 0x7
	jal	f
	move	$3,$2
	li	$2,-8			# 0xfffffffffffffff8
	bne	$3,$2,$L2
	li	$4,15			# 0xf
	jal	f
	bne	$2,$0,$L2
	li	$4,3			# 0x3
	jal	f
	move	$3,$2
	li	$2,12			# 0xc
	beq	$3,$2,$L3
$L2:
	li	$2,1			# 0x1
	b	$L5
$L3:
	move	$2,$0
$L5:
	move	$sp,$fp
	lw	$31,28($sp)
	lw	$fp,24($sp)
	addiu	$sp,$sp,32
	j	$31
	.end	main
	.size	main, .-main
	.ident	"GCC: (GNU) 10.2.1 20201203"
