;;; assembly compiled from file: test/programs/naires.scm
 PUSH PRIM 5
 CALL 0
 PUSH INT 3
 PUSH PRIM 6
 CALL 2
 PUSH INT 2
 PUSH PRIM 6
 CALL 2
 PUSH INT 1
 PUSH PRIM 6
 CALL 2
 JUMP L1
L0:
 FETCH 0
 PUSH INT 0
 PUSH PRIM 6
 CALL 2
 RETURN
L1:
 PUSH FUN L0
 CALL 1
 POP
