#define SERIAL ((volatile char *) 0x10001000ULL)

void print(int num) {
  int q;
  char tmp[10];
  char *cur = tmp;
  if(num == 0) {
    *SERIAL = '0';
    *SERIAL = '\n';
  } else {
    while(num) {
      *cur = (num % 10) + '0';
      ++cur;
      num = num / 10;
    }
    while(cur != tmp) {
      cur--;
      *SERIAL = *cur;
    }
    *SERIAL = '\n';
  }
}

int main() {
  int a = 0, b = 1, cnt = 0, c;
  for(; cnt < 10; ++cnt) {
    print(a);
    c = a + b;
    a = b;
    b = c;
  }
}
