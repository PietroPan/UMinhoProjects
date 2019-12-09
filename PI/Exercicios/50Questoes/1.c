#include <stdio.h>

int main () {
    int a=0,b=1;
    while (b!=0) {
        scanf ("%d",&b);
        if (b > a) (a=b);
    }
    printf ("%d",a);
return 0;
}