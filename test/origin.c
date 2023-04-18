int recv() { 
    int x;
    return x;
}

int constructReply() {
    int y;
    return y;
}

void send (int z) {

}
void log(int b) {}

void C1(int x, int c) {

  while(x>0) {
    int b = recv();
    if (c>0) log(b);
    if (b>0) {
      int n = constructReply();
      send(n);
      if (c>0) log(n);
    }
    x = x - 1;
  }
}

int main() {
  int x, c;
  C1(x, c);
  return 0;
}

