cd test
llvm-as test.ll
llc test.bc
clang test.s
./a.out
echo $?
cd ../