#include <stdio.h>
#include <stdlib.h>

#define V 5

typedef int GraphMat[V][V];

typedef struct edge {
    int dest;
    //int cost;
    struct edge *next;
}*EList;

typedef EList Graph[V];

void printMat (GraphMat m) 
{
    int i,j;
    for (i=0;i<V;i++)
    {
        for (j=0;j<V;j++)
        {
            printf("%d ",m[i][j]);
        }
        printf("\n");
    }
}

void printG (Graph g)
{
    struct edge *x;
    int i;
    for (i=0;i<V;i++)
    {
        for (x=g[i];x!=NULL;x=x->next)
        {
            printf("%d ",x->dest);
        }
        printf("\n");
    }
}

int main () 
{
    GraphMat testM = {
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

    EList testG1=malloc(sizeof(struct edge));
    testG1->dest=1;
    testG1->next=NULL;

    EList testG2=malloc(sizeof(struct edge));
    testG2->dest=4;
    testG2->next=NULL;

    EList testG3=malloc(sizeof(struct edge));
    testG3->dest=0;
    testG3->next=malloc(sizeof(struct edge));
    testG3->next->dest=2;
    testG3->next->next=NULL;

    EList testG4=malloc(sizeof(struct edge));
    testG4->dest=3;
    testG4->next=NULL;

    Graph testG = {testG0,testG1,testG2,testG3,testG4};

    //printMat(testM);
    //printG(testG);
    return 0;
}

/*void matToList (GraphMat m,Graph g)
{
    int i, j;
    EList tmp;
    for (i=0;i<V;i++)
    {
        gd[i]=NULL;
        for (j=V-1;j>=0;j--)
        if (m[i][j] != )
    }
}*/