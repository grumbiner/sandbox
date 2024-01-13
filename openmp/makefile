CPP = /usr/local/opt/llvm/bin/clang

CPPFLAGS = -I/usr/local/opt/llvm/include -fopenmp -I/usr/local/opt/libomp/include 
LDFLAGS = -L/usr/local/opt/llvm/lib -L/usr/local/opt/libomp/lib -lomp 
#  LDFLAGS="-L/usr/local/opt/llvm/lib/c++ -Wl,-rpath,/usr/local/opt/llvm/lib/c++"

#echo 'export PATH="/usr/local/opt/llvm/bin:$PATH"' >> ~/.zshrc

hello: hello.c
	$(CPP) $(CPPFLAGS) $^ -o $@ $(LDFLAGS)
