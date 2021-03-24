
int f(int x) {
  int y = 5;
  switch(x) {
    case 1:
      y = 23;
      y += x;
    return y;
    case 2:
      x += y;
    case 3:
      x += y;
    break;
    default:
    return x;
  }
  return x;
}
