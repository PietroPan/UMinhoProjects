#include <stdio.h>

int main () {
    int a=0,b=0,c=0;
    scanf ("%d",&a);
    while (a!=0) {
        if (a>b) {(c=b);(b=a);}
        else if (a>c) (c=a);
    scanf ("%d",&a);
    }
    printf ("%d",c);
return 0;
}