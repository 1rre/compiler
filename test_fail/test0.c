int fn() {
  int y[3] = {5,10,15};
  *y = 20;
  *(y+1) = 25;
  *(y+2) = 30;
  return *y;
}
