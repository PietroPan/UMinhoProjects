#include <stdio.h>

int main () {
    int a=0,b=0,c=0;
    scanf ("%d",&a);
    while (a!=0) {
        b+=a;
        ++c;
        scanf ("%d",&a);
    }
    printf("%d",(b/c));
return 0;
}