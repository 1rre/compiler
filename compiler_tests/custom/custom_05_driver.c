int f(int, int);

int main(){
    return !(f(23, 45) && f(72, 124) && f(256, 512));
}