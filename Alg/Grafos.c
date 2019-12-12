#include <stdio.h>
#include <stdlib.h>

#define V 10

typedef int GraphMat[V][V];

typedef struct edge {
    int dest;
    //int cost;
    struct edge *next;
}*EList;

typedef EList Graph[V];

int main () 
{
    GraphMat testM[5][5] = {
        {0,0,1,1,0},
        {0,1,0,0,0},
        {0,0,0,0,1},
        {1,0,1,0,0},
        {0,0,0,1,0}
    };

    EList testG0=malloc(sizeof(struct edge));
    testG0->dest=2;
    testG0->next=malloc(sizeof(struct edge));
    testG0->next->dest=3;
    testG0->next->next=NULL;

    Graph testG[5] = {
        testG0
    };
    printf ("Hello Satan\n");
    return 0;
}