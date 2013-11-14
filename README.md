#User instruction:
 - L_speed will compile lisp like language L to assemble language, and then compile to 32 bit a.out executable
 - User may compile and execute L1 - L5 inside L_speed to see how L compiler works step by step
 	- L1: A language similar to x86 Assembly language, it has five register in use: eax, ebx, ecx, esi, edi
	- L2: L1 with variables. It now considers liveness, split and coloring(register assign)
	- L3: It now eliminates decode and encode, has basic semantics such as let
	- L4: Dynamic typed language which means "everything goes everywhere"
	- L5: Enhance with lambda, a temprary function. 
