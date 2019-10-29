#define SERIAL ((volatile char *) 0x100000)

void div10(int *value, int *q) {
  *q = 0;
  while(*value > 10) {
    (*q)++;
    *value -= 10;
  }
}

void print(int num) {
  int q;
  if(num == 0) {
    *SERIAL = '0';
    *SERIAL = '\n';
  } else {
    while(num) {
      div10(&num, &q);
      *SERIAL = num + '0';
      num = q;
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
