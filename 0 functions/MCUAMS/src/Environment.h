#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include <vector>
#include <string>
#include "MDP.h"

class Environment {
public:
    Environment(const std::string& filename, float discount, int n_trials);
    Environment(const Environment& other);
    ~Environment();

    bool setReward(const std::string& filename);

    float getReward(int state, int action) const;
    int getNStates() const;
    int getNActions() const;
    int getNTrials() const;

    // Function to fill staticList with random numbers between 0 and 1
    static std::vector<std::vector<float>> staticList;
    static void randomParameters();

    static std::vector<std::vector<float>> values;
    static void computeValues(const Environment& env);

    static std::vector<int> indexes;
    static void findMax(const Environment& env);

    static std::vector<std::vector<float>> meanPars;
    static std::vector<float> priors;

    static void meanParsFunction(const Environment &env);
private:
    int N_states;
    int N_actions;
    int N_trials;
    std::vector<std::vector<float>> reward;
    float discount;

};

#endif
