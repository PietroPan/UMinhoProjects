#include <stdio.h>
#include <stdlib.h>

#define V 8
#define MAX 5

typedef int GraphMat[V][V];

typedef struct edge {
    int dest;
    int cost;
    struct edge *next;
}*EList;

typedef EList Graph[V];

typedef struct queue {
    int values [MAX];
    int inicio,tamanho;
}*Queue;

void initQueue (Queue q)
{
    q->inicio=0;
    q->tamanho=0;
}

int isEmpty (Queue q)
{
    return (q->tamanho==0);
}

int enqueue (Queue q,int a)
{
    int r=0;
    if (q->tamanho==MAX) r=1;
    else 
    {
        q->values[((q->inicio+q->tamanho)%MAX)]=a;
        q->tamanho++;
    }
    return r;
}

int dequeue (Queue q,int *a)
{
    int r=0;
    if (q->tamanho==0) r=1;
    else
    {
        *a=q->values[q->inicio];
        q->inicio=(q->inicio+1)%MAX;
        q->tamanho--;
    }
    return r;
}

void printQueue (Queue q)
{
    int i;
    for (i=0;i<q->tamanho;i++)
    {
        printf("%d ",q->values[(i+q->inicio)%MAX]);
    }
}

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
        printf("%d",i);
        for (x=g[i];x!=NULL;x=x->next)
        {
            printf("--%d-->%d",x->cost,x->dest);
        }
        printf("\n");
    }
}

void matToList (GraphMat m,Graph g)
{
    int i, j;
    EList tmp;
    for (i=0;i<V;i++)
    {
        g[i]=NULL;
        for (j=V-1;j>=0;j--)
        if (m[i][j]) {
            tmp=malloc(sizeof(struct edge));
            tmp->dest=j;
            tmp->cost=m[i][j];
            tmp->next=g[i];
            g[i]=tmp;
        }
    }
}

int nArestas (Graph g)
{
    int i,r=0;
    struct edge *x;
    for (i=0;i<V;i++)
    {
        for (x=g[i];x!=NULL;x=x->next) r++;
    }
    return r;
}

int pesoAresta (Graph g,int a,int b) 
{
    struct edge *aux;
    for (aux=g[a];aux!=NULL && aux->dest!=b ;aux=aux->next);
    if (aux) return (aux->cost);
    else return 0;
}

int grauSaida (Graph g,int v)
{
    int r=0;
    struct edge *aux;
    for (aux=g[v];aux!=NULL;aux=aux->next) r++;
    return r;
}

int grauEntrada (Graph g,int v)
{
    int r=0,i;
    struct edge *aux;
    for (i=0;i<V;i++)
    {
        for (aux=g[v];aux!=NULL;aux=aux->next)
        {
            if (aux->dest==v) r++;
        }
    }
    return r;
}

int capacidade (Graph g,int v)
{
    int s=0,e=0,i;
    struct edge *aux;
    for (aux=g[v];aux!=NULL;aux=aux->next) e+=aux->cost;
    for (i=0;i<V;i++)
    {
        for (aux=g[i];aux!=NULL;aux=aux->next)
        {
            if (aux->dest==v) s+=aux->cost;
        }
    }
    return (e-s);
}

int maxCap (Graph g)
{
    int cap[V],i,r=0;
    struct edge *aux;
    for (i=0;i<V;i++)
    {
        for (aux=g[i];aux!=NULL;aux=aux->next)
        {
            cap[i]-=aux->cost;
            cap[aux->dest]+=aux->cost;
        }
    }
    for (i=1;i<V;i++)
    {
        if (cap[i]>cap[r]) r=i;
    }
    return r;
}

int vCoverage (Graph g,int c[])
{
    int i;
    struct edge *aux;
    for (i=0;i<V;i++)
    {
        for (aux=g[i];aux!=NULL;aux=aux->next)
        {
            if (c[i]==0 && c[aux->dest]==0) return 0;
        }
    }
    return 1;
}

int colorOK (Graph g, int cor[])
{
    int i;
    struct edge *aux;
    for (i=0;i<V;i++)
    {
        for (aux=g[i];aux!=NULL;aux=aux->next)
        {
            if (cor[i]==cor[aux->dest]) return 0;
        }
    }
    return 1;
}

int isOrd (int c[],int a,int b)
{
    int i;
    for (i=0;i<V;i++)
    {
        if (c[i]==a) return 1;
        if (c[i]==b) return 0;
    }
}

int testTop (Graph g,int v[])
{
    int i;
    struct edge *aux;
    for (i=0;i<V;i++)
    {
        for (aux=g[i];aux!=NULL;aux=aux->next)
        {
            if (!isOrd(v,i,aux->dest)) return 0;
        }
    }
    return 1;
}

int procuraRec (Graph g,int o,int d,int vis[])
{
    int found=0;
    struct edge *aux;
    vis[o]=1;
    if (o==d) found=1;
    for (aux=g[o];aux!=NULL&&!found;aux=aux->next)
    {
        if (!vis[aux->dest])
        found=(procuraRec (g,aux->dest,d,vis));
    }
    return found;
}

int procura (Graph g,int o,int d)
{
    int vis[V],i;
    for (i=0;i<V;i++) vis[i]=0;
    return (procuraRec(g,o,d,vis));
}

int travDFRec (Graph g,int o,int vis[])
{
    int count=1;
    struct edge *aux;
    vis[o]=1;
    for (aux=g[o];aux!=NULL;aux=aux->next)
    {
        if (!vis[aux->dest])
        count+=travDFRec(g,aux->dest,vis);
    }
    return count;
    
}

int travDF (Graph g,int o)
{
    int vis[V],i;
    for (i=0;i<V;i++) vis[i]=0;
    return (travDFRec(g,o,vis));
}

int maiorCL (Graph g)
{
    int i,vis[V],max=0,tmp=0;
    for (i=0;i<V;i++) vis[i]=0;
    for (i=0;i<V;i++)
    {
        if (!vis[i])
        {
            tmp=travDFRec(g,i,vis);
            if (tmp>max) max=tmp;
        }
    }
    return max;
}

int bipartidoRec (Graph g,int o,int c,int vis[])
{
    int r=1;
    struct edge *aux;
    vis[o]=c;
    for (aux=g[o];aux!=NULL;aux=aux->next)
    {
        if (vis[aux->dest]==vis[o]) 
        {
            r=0;
            break;
        }
        if (vis[aux->dest]<0)
        r=bipartidoRec(g,aux->dest,!c,vis);
    }
    return r;
}

int bipartido (Graph g)
{
    int vis[V],i;
    for (i=0;i<V;i++) vis[i]=-1;
    return (bipartidoRec(g,0,0,vis));
}

int travDFRec2 (Graph g,int o,int p[],int vis[])
{
    int count=1;
    struct edge *aux;
    vis[o]=1;
    for (aux=g[o];aux!=NULL;aux=aux->next)
    {
        if (!vis[aux->dest])
        {
            p[aux->dest]=o;
            count+=travDFRec2(g,aux->dest,p,vis);
        }
    }
    return count;
}

int travDF2 (Graph g,int o,int p[])
{
    int vis[V],i;
    for (i=0;i<V;i++) vis[i]=0;
    p[o]=-1;
    return (travDFRec2(g,o,p,vis));
}

int travBF (Graph g ,int o)
{
    int count=0,vis[V],i;
    struct edge *aux;
    Queue q=malloc(sizeof(struct queue));
    initQueue(q);

    for (i=0;i<V;i++) vis[i]=0;

    vis[o]=1;
    enqueue(q,o);
    while (!isEmpty(q))
    {
        dequeue(q,&o);
        count++;
        for (aux=g[o];aux!=NULL;aux=aux->next)
        {
            if(!vis[aux->dest])
            {
                enqueue(q,aux->dest);
                vis[aux->dest]=1;
            }
        }
    }
}

int travBFTree (Graph g,int o,int ant[])
{
    int count=0,vis[V],i;
    struct edge *aux;
    Queue q=malloc(sizeof(struct queue));
    initQueue(q);

    for (i=0;i<V;i++) vis[i]=0;

    ant[o]=-1;
    vis[o]=1;
    enqueue(q,o);
    while (!isEmpty(q))
    {
        dequeue(q,&o);
        count++;
        for (aux=g[o];aux!=NULL;aux=aux->next)
        {
            if(!vis[aux->dest])
            {
                ant[aux->dest]=o;
                enqueue(q,aux->dest);
                vis[aux->dest]=1;
            }
        }
    }
}

int adist (Graph g,int o,int k)
{
    int vis[V],i,dis[V],n=0,r=0;
    struct edge *aux;
    Queue q=malloc(sizeof(struct queue));
    initQueue(q);

    for (i=0;i<V;i++) vis[i]=0;
    dis[o]=n;
    vis[o]=1;
    enqueue(q,o);
    while (!isEmpty(q))
    {
        dequeue(q,&o);
        if(dis[o]==k) r++;
        for (aux=g[o];aux!=NULL;aux=aux->next)
        {
            if(!vis[aux->dest])
            {
                enqueue(q,aux->dest);
                vis[aux->dest]=1;
                dis[aux->dest]=dis[o]+1;
            }
        }
    }
    return r;
}

int haLigacao (Graph g,int o,int d,int sec)
{
    int vis[V],i,r;
    struct edge *aux;
    Queue q=malloc(sizeof(struct queue));
    initQueue(q);

    for (i=0;i<V;i++) vis[i]=0;

    vis[o]=1;
    enqueue(q,o);
    while (!isEmpty(q))
    {
        dequeue(q,&o);
        if (o==d) r=1;
        for (aux=g[o];aux!=NULL;aux=aux->next)
        {
            if((!vis[aux->dest])&&(aux->cost>sec))
            {
                enqueue(q,aux->dest);
                vis[aux->dest]=1;
            }
        }
    }
    return r;
}

int caminhoValido (Graph g, int o, int p[],int N)
{
    int vis[V],i,dis[V];
    struct edge *aux;
    Queue q=malloc(sizeof(struct queue));
    initQueue(q);

    for (i=0;i<V;i++) vis[i]=0;

    dis[o]=0;
    vis[o]=1;
    enqueue(q,o);
    while (!isEmpty(q))
    {
        dequeue(q,&o);
        if(dis[o]==N) return 1;
        for (aux=g[o];aux!=NULL;aux=aux->next)
        {
            if(!vis[aux->dest]&&aux->cost==p[dis[o]])
            {
                dis[aux->dest]=dis[o]+1;
                enqueue(q,aux->dest);
                vis[aux->dest]=1;
            }
        }
    }
    return 0;
}

int tkhanTS (Graph g ,int seq[])
{
    int i,ini=0,fim=0,o;
    int grau[V];
    struct edge *aux;
    for (i=0;i<V;i++) grau[i]=0;
    for (i=0;i<V;i++)
    {
        for (aux=g[i];aux!=NULL;aux=aux->next)
        {
            grau[aux->dest]++;
        }
    }
    for (i=0;i<V;i++)
    {
        if (grau[i]==0) seq[fim++]=i;
    }
    while(fim>ini)
    {
        o=seq[ini++];
        for (aux=g[o];aux!=NULL;aux=aux->next)
        {
            grau[aux->dest]--;
            if(grau[aux->dest]==0)
            {
                seq[fim++]=aux->dest;
            }
        }
    }
    for (i=0;i<ini;i++) printf("%d\n",seq[i]);
}

int main () 
{
    int p[V],i;
    int cam[3] = {6,9};
    int x;

    GraphMat testM = {
        {0,0,0,0,0,0,0,0},
        {1,0,1,0,0,0,0,0},
        {0,0,0,0,0,0,0,0},
        {1,0,0,0,1,0,1,0},
        {1,1,0,0,0,0,0,0},
        {0,1,1,0,0,0,0,1},
        {0,0,0,0,0,0,0,0},
        {0,0,0,1,0,0,1,0}
    };

    Graph testG;
    matToList (testM,testG);
    tkhanTS(testG,p);
    //printf("%d\n",tkhanTS(testG,p));

    //printf("R = %d\n",travDF2(testG,4,p));
    //for (i=0;i<V;i++) printf("%d\n",p[i]);
    //printMat(testM);
    //printG(testG);
    //printf("%d\n",nArestas(testG));
    return 0;
}
/*
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
*/