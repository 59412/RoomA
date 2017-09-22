#include <iostream>
#include <cstdlib>
#include <vector>
#include <queue>
# define LL long long int

using namespace std;

long long int city_total,road_total,city_start,city_end;
int money_total;

int main()
{
  cin >> city_total;
  cin >> money_total;
  cin >> road_total;
  
  vector<LL> start(road_total,0);
  vector<LL> ending(road_total,0);
  vector<int> price(road_total,0);
  
  vector<int> ajc_array[city_total + 1];
  int cost_array[city_total + 1][city_total + 1];
  
  for(int i = 0; i < road_total; i++)
  {
    cin >> start[i];
    cin >> ending[i];
    cin >> price[i];
    
    ajc_array[start[i]].emplace_back(ending[i]);
    cost_array[start[i]][ending[i]] = price[i];
  }
  
  cin >> city_start;
  cin >> city_end;
  
  queue<LL> q;
  LL parent[(city_total + 1) * (money_total + 1)] = {0};
  int lowest_cost[(city_total + 1)];
  LL distance[(city_total + 1) * (money_total + 1)] = {0};
  
  for(int i = 0; i < city_total + 1; i++) lowest_cost[i] = 11;
  
  q.push(city_start * (money_total + 1));
  distance[city_start * (money_total + 1)] = 0;
  
  while(!q.empty())
  {
    LL u = q.front();
    q.pop();
    
    for(int i = 0; i < (ajc_array[u / (money_total + 1)]).size(); i++)
    {
      LL v_nocost = (ajc_array[u / (money_total + 1)])[i];
      if(lowest_cost[v_nocost] > (u % (money_total + 1)) + cost_array[u / (money_total + 1)][v_nocost])
      {
        lowest_cost[v_nocost] = (u % (money_total + 1)) + cost_array[u / (money_total + 1)][v_nocost];
        if((lowest_cost[v_nocost]) > money_total) continue;
        
        LL v = v_nocost * (money_total + 1) + (u % (money_total + 1)) + cost_array[u / (money_total + 1)][v_nocost];
        q.push(v);
        parent[v] = u;
        distance[v] = distance[u] + 1;
        
        if(v_nocost == city_end)
        {
          cout << distance[v] << endl;
          LL curr_print = v;
          
          int print_queue[distance[v] + 1];
          
          for(int j = distance[v]; j >= 0; j--)
          {
            print_queue[j] = curr_print;
            if(j != 0) curr_print = parent[curr_print];
          }
          
          for(int k = 0; k <= distance[v];k++)
          {
            cout << (print_queue[k] / (money_total + 1)) << " ";
          }
          cout << endl;
          return 0;
        }
      }
    }
  }
    
  cout << -1 << endl;
  return 0;
}
