.globl arr
.bgnb arr
.data
arr$1$1:
.word 10
.word 20
arr$1$2:
.word 30
.word 40
arr$1:
.word arr$1$1
.word arr$1$2
arr:
.word arr$1
.endb

.text
.ent main
.globl main
main:
lw $v0,arr
lw $v0,($v0)
lw $v0,4($v0)
jr $ra
nop

.end main
