#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// 4)

int bitsUm (unsigned int n)
{
    int c=0;
    while (n!=0) 
    {
        if ((n%2)==1) ++c;
        n=(n/2);
    }
    return c;
}

// 5)

int trailingZ (unsigned int n) 
{
    int i,b=0;
    for ((i=1);(b==0);(++i)) (b=(n%(int)(pow(2,i))));
    return (i-2);
}

// 6)

int qDig (unsigned int n) 
{
    int i;
    for ((i=1);(n>=((int)(pow(10,i))));(i++));
    return i;
}

// 7)

char sstrcat (char s1[],char s2[]) 
{
    int a,b;
    for ((a=0);(s1[a]!='\0');(a++));
    for ((b=0);(s2[b]!='\0');(b++),(a++)) (s1[a]=s2[b]);
    s1[a]='\0';
    return s1;
}


// 8)

char *sstrcyp (char *dest,char source[]) 
{
    int i;
    char *dest0 = dest;
    for ((i=0);(source[i]!='\0');(i++)) (*dest++ = source[i]);
    *dest ='\0';
    return dest0;
}

// 9)

int sstrcmp (char s1[], char s2[]) 
{
    int i;
    for (i=0; (s1[i]!='\0' || s2[i]!='\0'); i++) 
    {
    if (s1[i] > s2[i]) return 1; else
    if (s1[i] < s2[i]) return (-1);
    }
    return 0;
}

// 10)

char sstrstr (char s1[],char s2[]) 
{
    int i,a,b;
    for (i=0;s1[i]!='\0';i++) 
    {
        b=0;
        if (s2[b]==s1[i]) for(a=i;s1[a]==s2[b];a++,b++) 
        {
            if (s2[b+1]=='\0') return i; 
        }
        b=0;
    }
    return s1;
}

// 11)

void sstrrev (char s[]) 
{
    int n = (strlen(s));
    char m[n];
    for (int i=0;s[i]!='\0';i++,n--) (m[n-1]=s[i]);
    for (int r=0;s[r]!='\0';r++) (s[r]=m[r]);
}

// 12)

void strnoV (char s[]) 
{
    int i=0;
    while (s[i]!='\0') 
    {
        if (s[i]==65||s[i]==69||s[i]==73||s[i]==79||s[i]==85||s[i]==97||s[i]==101||s[i]==105||s[i]==111||s[i]==117) 
        {
            for (i;s[i]!='\0';i++) s[i] = s[i+1];
            i=0;
        }
        else i++;
    }
}

// 13)

void truncW (char s[], int n) 
{
    int i,r=0,j=0;
    for (i=0;s[i]!='\0';i++) 
    {
        s[j]=s[i];
        if (s[i]==' ') r=0;
        else r++;
        j++;
        if (r==n) 
        {
            for (;s[i]!=' '&&s[i]!='\0';i++);
            i--;
        }
    }
    s[j]='\0';
}
// 14)

int contachar (char s[],char c) 
{
    int a=0,i;
    for (i=0;s[i]!='\0';i++) 
    {
        if (s[i]==c) ++a;
    }
    return a;
}

char charMaisfreq (char s[]) 
{
    int c=0,a=0,i;
    char b='\0';
    if (s[0]=='\0') return '\0';
    for (i=0;s[i]!='\0';i++) 
    {
        c=numchar(s,s[i]);
        if (a<c) {a=c;b=s[i];}
    }
    return b;
}

// 15)

int iguaisConsecutivos (char s[]) 
{
    int i,a=0,b=0;
    char c='\0';
    for (i=0;s[i]!='\0';i++) 
    {
        if (s[i]==c) b++;
        else 
        {
            c=s[i];
            b=1;
        }
        if (b>a) a=b;
    }
    return a;
}

// 16)

int difConsecutivos (char s[]) 
{
    int a=0,b=0,i;
    char c='\0';
    for (i=0;s[i]!='\0';i++) 
    {
        if (s[i]!=c) 
        {
            b++;
            c=s[i];
        }
        else 
        {
            b=1;
            c=s[i];
        }
        if (b>a) a=b;
    }
    return a;
}

// 17)

int maiorPrefixo (char s1 [], char s2 []) 
{
    int a=0;
    for (i=0;(s1[i]!='\0'&&s1[i]!='\0');i++)
    {
        if (s1[i]==s2[i]) a++;
        else return a;
    }
    return a;
}

// 18)

int maiorSufixo (char s1 [], char s2 []) 
{
    int i=0,r=0,a=0,b=0;
    for (;s1[a]!='\0';a++);
    for (;s2[b]!='\0';b++);
    if (a<b) i=a; else i=b;
    a--;
    b--;
    for (;i!=0;i--,a--,b--)
    {
        if (s1[a]==s2[b]) r++;
        else return r;
    }
    return r;
}

// 19)

int sufPref (char s1 [], char s2 []) 
{
    int b=0,a=0;
    for (int i=0;s1[i]!='\0';i++) 
    {
        if (s1[i]==s2[a]) 
        {
            a++;
            b=1;
        }
        else 
        {
            a=0;
            if (b==1) {i--;b=0;}
        }
    }
    return a;
}

// 20)

int contaPal (char s[]) 
{
    int a=0,i=0;
    for (;s[i]!='\0';i++) 
    {
        if (s[i]!=' ') 
        {
            a++;
            for (;s[i]!=' '&&s[i]!='\0';i++);
        }
    }
    return a;
}

// 21)

int contaVogais (char s[]) 
{
    int a=0;
    for (int i=0;s[i]!='\0';i++)
    {
        if (s[i]==65||s[i]==69||s[i]==73||s[i]==79||s[i]==85||s[i]==97||s[i]==101||s[i]==105||s[i]==111||s[i]==117) a++;
    }
    return a;
}

// 22)

int contida (char a[], char b[]) 
{
    for (int i=0;a[i]!='\0';i++) 
    {
        for (int l=0;b[l]!=a[i];l++)
        {
            if (b[l]=='\0') return 0; 
        }
    }
    return 1;
}

// 23)

int palindorome (char s[]) 
{
    int a,i;
    for (a=0;s[a]!='\0';a++);
    a--;
    int e=(a/2)+1;
    for (i=0;i!=e;a--,i++) 
    {
        if (s[i]!=s[a]) return 0;
    }
    return 1;
}

// 24)

int remRep (char s[]) 
{
    int a=0,b=0,i;
    char c='\0';
    for (i=0;s[i]!='\0';i++) 
    {
        if (s[i]!=c) 
        {
            c=s[i];
            s[a]=s[i];
            a++;
            b++;
        }
    }
    s[a]='\0';
    return b;
}

// 25)

int limpaEspacos (char s[]) 
{
    int a=0,b=0,i;
    char c='\0';
    for (i=0;s[i]!='\0';i++) 
    {
        if (s[i]!=' '||s[i+1]!=' ') 
        {
            c=s[i];
            s[a]=s[i];
            a++;
            b++;
        }
    }
    s[a]='\0';
    return b;
}

// 26)

void insere(int v[], int N, int x)
{
	for ( ; N > 0 && x < v[N-1]; --N) 
    {
		v[N] = v[N - 1];
        printf("\n");
    }
	v[N] = x;
}

// 27)

void merge (int r[], int a[], int b[], int na, int nb) 
{
    int an=0,bn=0;
    for(int i=0;i!=(na+nb);i++) 
    {
        if (an==na) 
        {
            r[i]=b[bn];bn++;
        }
        else  if (bn==nb) 
        {
            r[i]=a[an];an++;
        }
        else if (b[bn]>a[an])
        {
            (r[i]=a[an]);
            an++;
        }
        else 
        {
            r[i]=b[bn];
            bn++;
        }
    }
}

// 28)

int crescente (int a[], int i, int j) 
{
    for (;i!=j;i++) 
    {
        if (a[i]>a[i+1]) return 0;
    }
    return 1;
}

// 29)

int retiraNeg (int v[], int N) 
{
    int a=0,i;
    for (i=0;i!=N;i++) 
    {
        if (v[i]>=0) 
        {
            v[a]=v[i];
            a++;
        }
    }
    return a;
}

// 30)

// versão alternativa da funcão contaChar definida em 14
int contaInt (int v[],int N,int a) 
{
    int b=0;
    for (int i=0;i!=N;i++) 
    {
        if (v[i]==a) b++;
    }
    return b;
}

int menosFreq (int v[],int N) 
{
    int n=contaInt(v,N,v[0]);
    int a=v[0];
    int b=0,i;
    for (i=1;i!=N;i++) 
    {
        if (v[i]!=a) 
        {
            b=contaInt(v,N,v[i]);
            if (b<n) 
            {
                n=b;
                a=v[i];
            }
        }
    }
    return a;
}

// 31)

// contaInt definida anteriormente em 30

int maisFreq (int v[],int N) 
{
    int n=contaInt(v,N,v[0]);
    int a=v[0];
    int b=0,i;
    for (i=1;i!=N;i++) 
    {
        if (v[i]!=a) 
        {
            b=contaInt(v,N,v[i]);
            if (b>n) 
            {
                n=b;
                a=v[i];
            }
        }
    }
    return a;
}

// 32)

int maxCresc (int v[], int N) 
{
    int a=1,=1,i=0;
    for (i=1;i!=N;i++) 
    {
        if (v[i]>=v[i-1]) b++;
        else b=1;
        if (b>a) a=b;
    }
    return a;
}

// 33)

int elimRep (int v[], int n) 
{
    int d=0,r=n,i;
    for (i=0;i!=n;i++) 
    {
        d=aux(v,n,v[i],i);
        r-=d;
        n-=d;
    }
    return r;
}

// 34)

int elimRepOrd (int v[], int n) 
{
    int a=1,i;
    for (i=1; i!=n;i++) 
    {
        if (v[i]!=v[i-1]) 
        {
            v[a]=v[i];a++;
        }
    }
    return a;
}

// 35)

int comunsOrd (int a[], int na, int b[], int nb) 
{
    int n=0,r=0,i:
    for (i=0;(n!=na&&i!=nb);i++) 
    {
        if (a[n]==b[i]) 
        {
            r++;
            n++;
        }
        else if (a[n]<b[i+1]) 
        {
            n++;
            i--;
        }
    }
    return r;
}

// 36)

int comum (int a,int s[],int n) 
{
    int i;
    for (i=0;i!=n;i++) 
    {
        if (s[i]==a) return 1;
    }
    return 0;
}

int comuns (int a[], int na, int b[], int nb) 
{
    int r=0,i;
    for (i=0;i!=na;i++) 
    {
        r+=comum(a[i],b,nb);
    }
    return r;
}

// 37)

int minInd (int v[], int n) 
{
    int a=v[0],r=0,i;
    for (i=1;i!=n;i++) 
    {
        if (v[i]<a) 
        {
            a=v[i];
            r=i;
        } 
    }
    return r;
}

// 38)

int somaux (int v[],int n) 
{
    int r=0;
    for (;n>=0;n--) r+=v[n];
    return r;
}

void somasAc (int v[], int Ac[], int N) 
{
    for (int i=0;i!=N;i++) (Ac[i]=somaux(v,i));
}

// 39)

int triSup (int N, float m [N][N]) 
{
    int i,j=0,a=1;
    for (i=1;i!=N;i++) 
    {
        for (j=0;j!=a;j++) 
        {
            if (m[i][j]!=0) return 0;
        }
        a++;
    }
    return 1;
}

// 40)

void transposta (int N, float m [N][N]) 
{
    int a=0,b=1,j,i;
    for(i=0;i!=(N-1);i++) 
    {
        for(j=1;j!=N;j++) 
        {
            a=m[i][j];
            m[i][j]=m[j][i];
            m[j][i]=a;
        }
        b++;
        j=b;
    }
}

// 41)

void addTo (int N, int M, int a[N][M], int b[N][M]) 
{
    int i;
    for (i=0;i!=N;i++) 
    {
        for (int j=0;j!=M;j++) (a[i][j]+=b[i][j]);
    }
}

// 42)

int unionSet (int N, int v1[N], int v2[N], int r[N]) 
{
    int i;
    for (i=0;i!=N;i++) 
    {
        if ((v1[i]+v2[i])==0) r[i]=0;
        else r[i]=1;
    }
}

// 43)

int intersectSet (int N, int v1[N], int v2[N], int r[N]) 
{
    int i;
    for (i=0;i!=N;i++) {
        if ((v1[i]*v2[i])==1) r[i]=1;
        else r[i]=0;
    }
}

// 44)

int intersectMSet (int N, int v1[N], int v2[N], int r[N]) 
{
    int i;
    for (i=0;i!=N;i++) 
    {
        if ((v1[i]<v2[i])==1) r[i]=v1[i];
        else r[i]=v2[i];
    }
}

// 45)

int intersectMSet (int N, int v1[N], int v2[N], int r[N]) 
{
    int i;
    for (i=0;i!=N;i++) {
        if ((v1[i]>v2[i])==1) r[i]=v1[i];
        else r[i]=v2[i];
    }
}

// 46)

int cardinalMSet (int N, int v[N]) 
{
    int r=0,i;
    for (i=0;i!=N;i++) r+=v[i];
    return r;
}

// estutura para as restantes questões

typedef enum movimento {Norte, Oeste, Sul, Este} Movimento;
    
typedef struct posicao 
{
    int x, y;
} Posicao;

// 47)

Posicao posFinal (Posicao inicial, Movimento mov[], int N) 
{
    int i;
    for (i=0;i!=N;i++) 
    {
        switch (mov[i])
        {
            case Norte: ++inicial.y;
                break;
            case Oeste: --inicial.x;
                break;
            case Sul: --inicial.y;
                break;
            case Este: ++inicial.x;
                break;
            default:
                break;
        }
    }
    return inicial;
}

// 48)

int caminho (Posicao inicial, Posicao final, Movimento mov[], int N) 
{
    int a=0,b=0,r=0;
    if (inicial.x > final.x) a+=(inicial.x-final.x);
    else a+=(final.x-inicial.x);
    if (inicial.y > final.y) b+=(inicial.y-final.y);
    else b+=(final.y-inicial.y);
    if (N < (a+b)) return -1;
    for (int i=0;i!=a;i++) 
    {
        if (inicial.x > final.x) mov[i]=Oeste;
        else mov[i]=Este;
        r++;
    }
    for (int j=a;j!=(a+b);j++) 
    {
        if (inicial.y > final.y) mov[j]=Sul;
        else mov[j]=Norte;
        r++;
    }
    return r;
}

// 49)

int maisCentral (Posicao pos[], int N) 
{
    int a=abs(pos[0].x)+abs(pos[0].y);
    int ia=0,b=0,i;
    for (i=1;i!=N;i++) 
    {
        b=abs(pos[i].x)+abs(pos[i].y);
        if (b < a) 
        {
            ia=i;
            a=b;
        }
    }
    return ia;
}

// 50)

int vizinhos (Posicao p, Posicao pos[], int N) 
{
    int r=0,i;
    for (i=0;i!=N;i++) 
    {
        if((pos[i].x+1)==p.x&&(pos[i].y==p.y)) r++;
        else if ((pos[i].x-1)==p.x&&(pos[i].y==p.y)) r++;
        else if ((pos[i].y+1)==p.y&&(pos[i].x==p.x)) r++;
        else if ((pos[i].y-1)==p.y&&(pos[i].x==p.x)) r++;
    }
    return r;
}

int main () 
{
    printf("That's all Folks");
    return 0;
}