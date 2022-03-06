#include <iostream>
#include<random>
#include<vector>
#include<math.h>
#include <algorithm>
#include <fstream>
#include <stdio.h>

std::mt19937_64 g_randomGenerator;
float pi = 4 * atan(1);

template <class T>
void const printVec(const std::vector<T> vec,size_t l , const char* end = "\n")
{
    int i = 0;
    for(auto e: vec) {
        std::cout << e << " ";
        if(++i % l == 0)
            std::cout << std::endl;
    }
    std::cout << end;
}

std::vector<bool> generateRandomVector(size_t L){
    std::vector<bool> ret(L);
    for(int i = 0; i < L; i++)
        ret[i] = g_randomGenerator() % 2;
    return ret;
}

float rand01(int resolution = 10000) {
    return g_randomGenerator() % resolution / float(resolution); //basic, could be improved
}

template <class T>
std::vector<double> transform(const std::vector<T> &vec, size_t L, size_t l,double a, double b, int p)
{
    std::vector<double> x;
    for(int i = 0; i < L; i++)
    {
        double s = 0;
        for(int j = 0; j < l; j++)
        {
            s*=2;
            s+=vec[i+j];
        }
        i += l - 1;
        s =  a + s * (b - a) / (pow(2,l)-1);
        s = round(s*p) / p;
        x.push_back(s);
    }
    return x;
}

double evaluate(int function, const std::vector<bool> &v, size_t L, size_t l,double a, double b, int p)
{
    auto x = transform(v,L,l,a,b,p);
    double f = 0;
    switch(function)
    {
        case 1:
            for(double i : x)
                f += i*i;
        break;
        case 2:
            for(double i : x)
                f+=(-i)*sin(sqrt(std::abs(i)));
        break;
        case 3:
            f+= 10 * x.size();
            for(double i : x)
                f += i * i - 10 * cos(2 * pi * i);
        break;
        case 4:
            for(int i = 0; i < x.size(); i++)
                f += sin(x[i]) * pow((sin((i+1)*x[i]*x[i] / pi)),20);
            f *= -1;
        break;
    }
    f = round(f * p) / p;
    return f;
}

void selection(std::vector<std::vector<bool> > &P,std::vector<double> E, int k, int pressure, int popSize)
{
    const auto [minimum, maximum] = std::minmax_element(E.begin(), E.end());
    std::vector<double> F;
    double fs = 0;
    for(int i = 0; i < E.size(); i++)
    {
        F.push_back(pow(((*maximum - E[i]) / (*maximum - *minimum + 0.000001) + 0.01),   pressure));
        fs += F[i];
    }
    std::vector<double> pc;
    pc.push_back(F[0]/fs);
    for(int i = 1; i < F.size(); i++)
    {
        pc.push_back(F[i] / fs + pc[i-1]);
    }
    std::vector<std::vector<bool> > Pnext;
    for(int i = 0; i < popSize - k; i++)
    {
        float r = rand01();
        bool chosen = false;
        for(int j = 0; j < pc.size(); j++) {
            if (r <= pc[j]) {
                Pnext.push_back(P[j]);
                chosen = true;
                break;
            }
        }
        if(chosen == false)
            Pnext.push_back(P[P.size()-1]);
    }
    P = Pnext;
}

void mutate(std::vector<std::vector<bool> > &Pt)
{
    for(int i = 0; i < Pt.size(); i++)
    {
        for(int j = 0; j < Pt[i].size(); j++)
        {
            float r = rand01();
            if(r <= 0.01)
                Pt[i][j] = !Pt[i][j];
        }
    }

}

bool compare(std::pair<int,double> i, std::pair<int,double> j)
{
    return i.second < j.second;
}

std::pair <std::vector<bool>, std::vector<bool> > cx(const std::vector<bool>&c1, const std::vector<bool>& c2)
{
    auto d1 = c1, d2 = c2;
    int pos = 1 + g_randomGenerator() % (c1.size() - 3);
    for (int i = pos; i < c1.size(); i++) {
        d1[i] = c2[i];
        d2[i] = c1[i];
    }
    return std::make_pair(d1,d2);
}

void crossover(std::vector<std::vector<bool> >& Pt, int &popSize) {
    std::vector<std::pair<int, float> > p;
    for (int i = 0; i < Pt.size(); i++)
        p.push_back(std::make_pair(i, rand01()));
    std::sort(p.begin(), p.end(), compare);
    int i = 0;
    for (i = 0; i < p.size(); i+=2)
    {
        if(i+1 == p.size() || p[i+1].second >= 0.6)
            break;
        auto x = cx(Pt[p[i].first],Pt[p[i+1].first]);
        Pt[p[i].first] = x.first;
        Pt[p[i+1].first] = x.second;

    }
    if(p[i].second < 0.6)
    {
        float r = rand01();
        if(r >= 0.5) {
            auto x = cx(Pt[p[i].first], Pt[p[i + 1].first]);
            Pt[p[i].first] = x.first;
            Pt[p[i + 1].first] = x.second;
        }
    }
}

std::vector<std::vector<bool> > elitism(const std::vector<std::vector<bool> > &Pt, const std::vector<double> &E, const int &k)
{
    std::vector<std::pair<int, float> > p;
    std::vector<std::vector<bool> > elit;
    for(int i = 0; i < E.size(); i++)
    {
        p.push_back(std::make_pair(i,E[i]));
    }
    std::sort(p.begin(), p.end(), compare);
    for(int i = 0; i < k; i++)
    {
        elit.push_back(Pt[p[i].first]);
    }
    return elit;
}

void eval(std::vector<double> &E,int function, const std::vector<std::vector<bool> > Pt, size_t L, size_t l,double a, double b, int p)
{
    for(int i = 0; i < Pt.size(); i++)
    {
        E[i] = evaluate(function,Pt[i],L,l,a,b,p);
    }
}

double ag(const int &function, int generations, int popSize , const size_t &L, const size_t &l,const double &a, const double &b, const int &p)
{
    int k = int(popSize*0.07),pressure = 4;
    int t = 0;
    std::vector<std::vector<bool> > Pt;
    std::vector<double> E;
    for(int i = 0; i < popSize; i++) {
       Pt.push_back(generateRandomVector(L));
       E.push_back(evaluate(function,Pt[i],L,l,a,b,p));
    }
    while(t < generations) {
        t++;
        auto elit = elitism(Pt,E,k);
        selection(Pt, E,k, pressure,popSize);
        mutate(Pt);
        crossover(Pt,popSize);
        for(int i = 0; i < elit.size(); i++)
        {
            Pt.push_back(elit[i]);
        }
        eval(E,function,Pt,L,l,a,b,p);
    }
    const auto[minimum, maximum] = std::minmax_element(E.begin(), E.end());
    return *minimum;
}


int main(int argc,char* argv[]) {
    //////////////
    // Apel de forma: ./main Function Dimension Precision Number_of_Generations PopSize
    //////////////
    auto start = std::chrono::steady_clock::now();
    int function,method;
    double a, b;
    if(argc != 6)
    {
        perror("Not enough parameters\nFormat: ./main Function Dimension Precision Number_of_Generations PopSize");
        return 0;
    }

//Function
    if(strcmp(argv[1],"DeJong") == 0) {
        function = 1;
        a = -5.12;
        b = 5.12;
    }else if(strcmp(argv[1],"Schwefel") == 0){
        function = 2;
        a = -500;
        b = 500;
    }else if(strcmp(argv[1],"Rastrigin") == 0){
        function = 3;
        a = -5.12;
        b = 5.12;
    }else if(strcmp(argv[1],"Michalewicz") == 0){
        function = 4;
        a = 0;
        b = pi;
    }
    else
    {
        perror("Incorrect function");
        return 0;
    }

// Dimension & Precision & Number of iterations
    int D = atoi(argv[2]),
        p = pow(10,atoi(argv[3])),
        generations = atoi(argv[4]),
        popSize = atoi(argv[5]);


    size_t l = ceil(log2(p*(b-a)));
    size_t L = D*l;
    int i = 1, maxit = 30;
    auto x = ag(function, generations, popSize, L, l, a, b, p);
    double min = x, average = x;
    while(i < maxit) {
        x = ag(function, generations, popSize, L, l, a, b, p);
        average+=x;
        if(min > x)
            min = x;
        i++;
    }
    std::cout << "Best: " << min << "\n" << "Average: " << average/maxit << "\n";
    auto end = std::chrono::steady_clock::now();
    std::cout << "Elapsed time in seconds: " << std::chrono::duration_cast<std::chrono::seconds>(end - start).count() / maxit<< " sec\n";

    return 0;
}

