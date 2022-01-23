{}
%wrapper "basic"

tokens :-

(32|64|128)[x]? ;
1234 ;
[1234] 56 ;

{
main = return ()
}
