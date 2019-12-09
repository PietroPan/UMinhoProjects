#include <stdio.h>
#include <stdlib.h>

// Estrutura de listas ligadas

typedef struct lligada {
    int valor;
    struct lligada *prox;
} *LInt;

// 1)

int length (LInt p) 
{
    int r=0;
    while (p!=NULL) 
    {
        r++;
        p=p->prox;
    }
    return r;
}

// 2)

void freeL (LInt l) 
{
    LInt *l1;
    while (l) 
    {
        l1=&(l->prox);
        free(l);
        l=*l1;
    }
    l=NULL;
}

// 3)

void imprimeL (LInt p) 
{
    while (p!=NULL) 
    {
        printf("%d\n",p->valor);
        p=p->prox;
    }
}

// 4)

LInt reverseL (LInt l) 
{
    LInt l2,l1=NULL;
    while(l) 
    {
        l2=l->prox;
        l->prox=l1;
        l1=l;
        l=l2;
    }
    return l1;
}

// 5)

void insertOrd (LInt *p,int n) 
{
    LInt new = malloc (sizeof (struct lligada));
    new->valor = n;
    while (*p && (*p)->valor<n) p=&((*p)->prox);
    new->prox = *p;
    *p=new;
}

// 6)

int removeOneOrd (LInt *p, int n) 
{
    LInt aux;
    while (*p) {
        if ((*p)->valor==n) 
        {
            aux=(*p)->prox;
            free(*p);
            *p=aux;
            return 0;
        }
        if ((*p)->valor>n) return 1;
        if ((*p)->valor<n) (p=&((*p)->prox));
    }
    return 1;
}

// 7)

void merge (LInt *r, LInt a, LInt b) 
{
    while (a||b) 
    {
        if ((!b)||a&&(a->valor<b->valor)) 
        {
            *r=a;
            r=&((*r)->prox);
            a=a->prox;
        }
        else 
        {
            *r=b;
            r=&((*r)->prox);
            b=b->prox;
        }
    }
}

// 8)

void splitQS (LInt l, int x, LInt *mx, LInt *Mx) 
{
    while (l) 
    {
        if (l->valor<x) 
        {
            *mx=l;
            mx=&((*mx)->prox);
        }
        else 
        {
            *Mx=l;
            Mx=&((*Mx)->prox);
        }
        l=l->prox;
    }
    *mx=NULL;
    *Mx=NULL;
}

// 9)

LInt parteAmeio (LInt *l) 
{
    int i,p=length(*l)/2;
    LInt r;
    LInt *aux=&r;
    for (i=0;i!=p;i++) 
    {
        *aux=*l;
        aux=&(*aux)->prox;
        *l=(*l)->prox;
    }
    *aux=NULL;
    return r;
}

// 10)

int removeAll (LInt *l, int n) 
{
    LInt aux;
    int r=0;
    while (*l) 
    {
        if ((*l)->valor==n) 
        {
            aux=(*l)->prox;
            free(*l);
            *l=aux;
            r++;
        }
        else l=&((*l)->prox);
    }
    return r;
}

// 11)

int removeDups (LInt *l) 
{
    int n;
    int r=0;
    while (*l) 
    {
        n=(*l)->valor;
        l=&((*l)->prox);
        r+=removeAll(l,n);
    }
    return r; 
}

// 12)

int removeMaiorL (LInt *l) 
{
    int r=(*l)->valor;
    LInt *c,aux;
    l=&((*l)->prox);
    c=l;
    while (*c) 
    {
        if ((*c)->valor>r) 
        {
            r=(*c)->valor;
            l=c;
        }
        c=&((*c)->prox);
    }
    aux=(*l)->prox;
    free(*l);
    *l=aux;
    return r;
}

// 13)

void init (LInt *l) 
{
    while((*l)->prox) l=&((*l)->prox);
    free(*l);
    *l=NULL;
}

// 14)

void appendL (LInt *l, int n) 
{
    LInt new=malloc(sizeof(struct lligada));
    new->prox=NULL;
    new->valor=n;
    while (*l) l=&((*l)->prox);
    *l=new;
}

// 15)

void concatL (LInt *a, LInt b) 
{
    while (*a) a=&((*a)->prox);
    *a=b;
}

// 16)

LInt cloneL (LInt l) 
{
    LInt r,*new=&r;
    while (l) 
    {
        *new=malloc(sizeof(struct lligada));
        (*new)->valor=l->valor;
        new=&((*new)->prox);
        l=l->prox;
    }
    *new=NULL;
    return r;
}

// 17)

LInt cloneRev (LInt l) 
{
    l=reverseL(l);
    cloneL(l);
}

// 18)

int maximo (LInt l) 
{
    int r=l->valor;
    l=l->prox;
    while (l) 
    {
        if ((l->valor)>r) r=l->valor;
        l=l->prox;
    }
    return r;
}

// 19)

int take (int n, LInt *l) 
{
    int r=0;
    while (*l&&n!=0) 
    {
        n--;
        l=&((*l)->prox);
        r++;
    }
    freeL(*l);
    return r;
}

// 20)

int drop (int n, LInt *l) 
{
    LInt aux;
    int r=0;
    while (*l&&r!=n) 
    {
        r++;
        aux=(*l)->prox;
        free(*l);
        *l=aux;
    }
    return r;
}

// 21)

LInt NForward (LInt l, int N) 
{
    while (l&&N!=0) 
    {
        N--;
        l=l->prox;
    }
    return l;
}

// 22)

int listToArray (LInt l, int v[], int N) 
{
    int i=0;
    while (l&&N!=0) 
    {
        v[i]=l->valor;
        l=l->prox;
        i++;
        N--;
    }
    return i;
}

// 23)

LInt arrayToList (int v[], int N) 
{
    LInt r,*new=&r;
    int i=0;
    while (i<N) 
    {
        *new=malloc(sizeof(struct lligada));
        (*new)->valor=v[i];
        new=&((*new)->prox);
        i++;
    }
    *new=NULL;
    return r;
}

// 24)

LInt somasAcL (LInt l) 
{
    LInt r,*new=&r;
    int acc=0;
    while (l) 
    {
        acc+=l->valor;
        *new=malloc(sizeof(struct lligada));
        (*new)->valor=acc;
        new=&((*new)->prox);
        l=l->prox;
    }
    *new=NULL;
    return r;
}

// 25)

void remreps (LInt l) 
{
    LInt p,j;
    while(l) 
    {
        p=l->prox;
        while(p&&l->valor==p->valor) 
        {
            j=p->prox;
            free(p);
            p=j;
        }
        l->prox=p;
        l=l->prox;
    }
}

// 26)

LInt rotateL (LInt l)
{
    LInt *p=&l,m=NULL;
    while(*p)
    p=&((*p)->prox);
    *p=l;
    if(l)
    {
        m=l->prox;
        l->prox=0;
    }
    return m;
}

// 27)

LInt parte (LInt l) 
{
    LInt r=NULL,aux,*new=&r,*new2=&l;
    int i=1;
    while (l) 
    {
        if (i==0) 
        {
            (*new)=malloc(sizeof(struct lligada));
            (*new)->valor=l->valor;
            new=&((*new)->prox);
            removeOneOrd(&l,l->valor);
            i=1;
        }
        else 
        {
            i=0;
            new2=&((*new2)->prox);
        }
    }
    return r;
}