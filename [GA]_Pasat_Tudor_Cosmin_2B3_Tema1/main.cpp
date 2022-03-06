#include <iostream>
#include<random>
#include<vector>
#include<math.h>

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

template <class T>
std::vector<double> transform(const std::vector<T> vec, size_t L, size_t l,double a, double b, int p)
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

double evaluate(int function, const std::vector<bool> v, size_t L, size_t l,double a, double b, int p)
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
                f += sin(x[i]) * pow((sin(i*x[i]*x[i] / pi)),20);
            f *= -1;
        break;
    }
    f = round(f * p) / p;
    return f;
}

std::vector<bool> FirstImprovement(int function,std::vector<bool> vc, size_t L, size_t l,double a, double b, int p)
{
    double solution = evaluate(function,vc,L,l,a,b,p);
    for(int i = 0; i < vc.size(); i++)
    {
        vc[i] = !vc[i];
        if(solution > evaluate(function,vc,L,l,a,b,p))
            return vc;
        vc[i] = !vc[i];
    }
    return vc;
}

std::vector<bool> BestImprovement(int function,std::vector<bool> vc, size_t L, size_t l,double a, double b, int p)
{
    auto candidate = vc;
    double solution = evaluate(function,candidate,L,l,a,b,p);
    double s;
    for(int i = 0; i < vc.size(); i++)
    {
        vc[i] = !vc[i];
        if(solution > (s = evaluate(function,vc,L,l,a,b,p))) {
            solution = s;
            candidate = vc;
        }
        vc[i] = !vc[i];
    }
    return candidate;
}

std::vector<bool> Improve(int type, int function,std::vector<bool> vc, size_t L, size_t l,double a, double b,int p)
{
    if(type == 0)
        return FirstImprovement(function,vc,L,l,a,b,p);
    else
        return BestImprovement(function,vc,L,l,a,b,p);
}

std::vector<bool> hillclimb(int type, int function, size_t L, size_t l,double a, double b, int maxi, int p)
{
    int t = 0;
    auto best = generateRandomVector(L);
    double fbest = evaluate(function,best,L,l,a,b,p);
    do{
        bool local = false;
        auto vc = generateRandomVector(L);
        double fvc = evaluate(function, vc,L,l,a,b,p);
        do{
            auto vn = Improve(type,function,vc,L,l,a,b,p);
            double fvn = evaluate(function, vn,L,l,a,b,p);
            if(fvc > fvn) {
                vc = vn;
                fvc = fvn;
            }
            else
                local = true;
        }while(!local);
        t++;
        if(fvc < fbest) {
            best = vc;
            fbest = fvc;
        }

    }while(t != maxi);
    std::cout << "Function minimum: " << fbest << std::endl;
    return best;
}


std::vector<bool> SimulatedAnnealing(int type, int function, size_t L, size_t l,double a, double b,int maxi, int p)
{
    int t = 0,i;
    double T = 1000;
    auto vc = generateRandomVector(L);
    double fc = evaluate(function,vc,L,l,a,b,p);
    auto best = vc;
    int lastsolution = -2, iterator = 0;
    double fbest = fc;

    std::vector<int> visitedn;
    for(i = 0; i < L; i++)
        visitedn.push_back(i);
    std::shuffle ( visitedn.begin(), visitedn.end(), std::mt19937(std::random_device()()));

        do{
            i=0;
            iterator = 0;
            do
            {
                auto vn = vc;
                if(lastsolution == visitedn[i]){
                    i++;
                    continue;
                }
                vn[visitedn[i]] = !vn[visitedn[i]];
                double fn = evaluate(function,vn,L,l,a,b,p);
                double probability = double((g_randomGenerator() % 10000))/10000;
                if(fn < fc){
                    fc = fn;
                    vc = vn;
                    lastsolution = visitedn[i];
                    std::shuffle ( visitedn.begin(), visitedn.end(), std::mt19937(std::random_device()()));
                    i = -1;
                }else if(probability < exp(-abs(fn-fc)/T))
                {
                    lastsolution = visitedn[i];
                    fc = fn;
                    vc = vn;
                    std::shuffle ( visitedn.begin(), visitedn.end(), std::mt19937(std::random_device()()));
                    i = -1;
                }
                i++;
                iterator++;
            }while(iterator < maxi && i < visitedn.size());
            T *= 0.9;
            t = t + 1;

        }while(T > 0.00005);

    std::cout << "Function minimum: " << fc << std::endl;

    return vc;

}

int main(int argc,char* argv[]) {
    //////////////
    // Apel de forma: ./main Method Function Dimension Precision Number_of_Iterations
    //////////////
    auto start = std::chrono::steady_clock::now();
    int function = 1,method;
    double a, b;
    int type;
    if(argc != 6)
    {
        perror("Not enough parameters");
        return 0;
    }

//Method
    if(strcmp(argv[1],"FIHC") == 0){
        method = 0;
        type = 0;
    }
    else if(strcmp(argv[1],"BIHC") == 0){
        method = 0;
        type = 1;
    }
    else if(strcmp(argv[1],"SA") == 0)
        method = 1;
    else
    {
        perror("Incorrect method");
        return 0;
    }


//Function
    if(strcmp(argv[2],"DeJong") == 0) {
        function = 1;
        a = -5.12;
        b = 5.12;
    }else if(strcmp(argv[2],"Schwefel") == 0){
        function = 2;
        a = -500;
        b = 500;
    }else if(strcmp(argv[2],"Rastrigin") == 0){
        function = 3;
        a = -5.12;
        b = 5.12;
    }else if(strcmp(argv[2],"Michalewicz") == 0){
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
    int D = atoi(argv[3]),
        p = pow(10,atoi(argv[4])),
        maxi = atoi(argv[5]);

    size_t l = ceil(log2(p*(b-a)));
    size_t L = D*l;

    if(method == 0) {
        auto vc = hillclimb(type, function, L, l, a, b, maxi, p);
        std::cout << "Solutions: \n";
        auto x = transform(vc, L, l, a, b, p);
        for (auto i : x)
            std::cout << i << " ";
        std::cout << std::endl;
    }
    else {
        auto vc = SimulatedAnnealing(0, function, L, l, a, b, maxi, p);
        std::cout << "Solutions: \n";
        auto x = transform(vc, L, l, a, b, p);
        for (auto i : x)
            std::cout << i << " ";
        std::cout << std::endl;
    }

    auto end = std::chrono::steady_clock::now();
    std::cout << "Elapsed time in seconds: " << std::chrono::duration_cast<std::chrono::seconds>(end - start).count() << " sec\n";
    std::cout << "Elapsed time in minutes: " << std::chrono::duration_cast<std::chrono::minutes>(end - start).count() << " min\n";
    return 0;
}
