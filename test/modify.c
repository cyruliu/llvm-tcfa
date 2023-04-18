int recv(){int x; return x;}

void send(int z){}

int constructReply(){
	int y;
	return y;
}

int check(int b){
	return b;
}

int log(int b){
}

void C2(int x, int c) {
  while(x>0) {
    int b = recv();
    if (b>0) {
      int auth = check(b);
      if (auth>0) {
        int n = constructReply();
        send(n);
      }
    }
    else log(b);
    x = x - 1;
  }
}

int main() {
  int x, c;
  C2(x, c);
  return 0;
}

