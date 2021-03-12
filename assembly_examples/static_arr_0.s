.globl arr
.bgnb arr
.data
$1$1$1arr:
.word 1
.word 2
.word 3
$1$1$2arr:
.word 4
.word 5
.word 6
$1$2$1arr:
.word 7
.word 8
.word 9
$1$2$2arr:
.word 10
.word 11
.word 12
arr$1$1:
.word $1$1$1arr
.word $1$1$2arr
arr$1$2:
.word $1$2$1arr
.word $1$2$2arr
arr$1:
.word $1$1arr
.word $1$2arr
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
