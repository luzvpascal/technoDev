#include "Environment.h"
#include <iostream>
#include <random>
#include "CSVParser.h"

// Initialize the static list of floats (assuming 0.0 for each element)
std::vector<std::vector<float>> Environment::staticList;
std::vector<std::vector<float>> Environment::values;
std::vector<int> Environment::indexes;
std::vector<std::vector<float>> Environment::meanPars;
std::vector<float> Environment::priors;

Environment::Environment(const std::string& filename, float discount, int n_trials)
        : discount(discount), N_trials(n_trials) {
    if (setReward(filename)) {
        // Calculate N_states and N_actions based on the size of the reward matrix
        N_states = reward.size();
        if (N_states > 0) {
            N_actions = reward[0].size();
        } else {
            N_actions = 0;
        }

        // Initialize the static list with N_trials rows and N_states * N_actions columns
        staticList.clear();
        staticList.resize(N_trials, std::vector<float>(N_states * N_actions, 0.0f));
        // Initialize staticList when creating an Environment object
        randomParameters();

        // Initialize the static list with N_trials rows and N_states * N_actions columns
        values.clear();
        values.resize(N_trials, std::vector<float>(N_actions * N_actions, 0.0f));

        indexes.clear();
        indexes.resize(N_trials, 0);

        meanPars.clear();
        meanPars.resize(N_actions * N_actions, std::vector<float>(N_states * N_actions, 0.0f));

        priors.clear();
        priors.resize(N_actions*N_actions, 0.0f);

    } else {
        // Default values if setReward fails
        N_states = 0;
        N_actions = 0;
        // Initialize the static list with default values
        staticList.clear();
        staticList.resize(n_trials, std::vector<float>(N_states * N_actions, 0.0f));
    }
}

Environment::Environment(const Environment& other)
        : N_states(other.N_states), N_actions(other.N_actions), N_trials(other.N_trials), discount(other.discount), reward(other.reward) {
}

Environment::~Environment() {
    // No dynamic memory to release, so no need to do anything here
}

bool Environment::setReward(const std::string& filename) {
    CSVParser csvParser(filename);
    std::vector<std::vector<std::string>> csvData;

    if (csvParser.readCSV(csvData)) {
        reward.clear(); // Clear the current reward matrix
        for (const auto& row : csvData) {
            std::vector<float> rowData;
            for (const auto& cell : row) {
                try {
                    rowData.push_back(std::stod(cell));
                } catch (const std::invalid_argument& e) {
                    std::cerr << "Error converting CSV data to float: " << e.what() << std::endl;
                    return false;
                }
            }
            reward.push_back(rowData);
        }
        return true;
    } else {
        std::cerr << "Failed to read the CSV file." << std::endl;
        return false;
    }
}

float Environment::getReward(int state, int action) const {
    if (state >= 0 && state < N_states && action >= 0 && action < N_actions) {
        return reward[state][action];
    } else {
        std::cerr << "Invalid state or action index. Returning 0.0." << std::endl;
        return 0.0;
    }
}

int Environment::getNStates() const {
    return N_states;
}

int Environment::getNActions() const {
    return N_actions;
}

int Environment::getNTrials() const {
    return N_trials;
}

//const MDP Environment::getMDP() const {
//    return mdp;
//}

// Function to fill staticList with random numbers between 0 and 1
void Environment::randomParameters() {
    std::random_device rd; // Seed for the random number generator
    std::mt19937 gen(rd()); // Mersenne Twister random number generator
    std::uniform_real_distribution<float> dist(0.0f, 1.0f); // Uniform distribution between 0 and 1

    // Fill staticList with random numbers
    for (auto& row : staticList) {
        for (auto& value : row) {
            value = dist(gen);
        }
    }
}

void Environment::computeValues(const Environment& env){
    //compute value function
  for (int n=0; n< env.N_trials; ++n){
      for (int act1 = 0; act1 < env.N_actions; ++act1){
          for (int act2 = 0; act2 < env.N_actions ; ++act2) {
              values[n][act1*env.N_actions + act2] = (env.reward[0][act1]*(1-env.discount+env.discount*staticList[n][act2*env.N_actions+1]) +
               env.reward[1][act2]*(env.discount-env.discount*staticList[n][act1*env.N_actions]))/((1-env.discount)*(1-staticList[n][act1*env.N_actions]*env.discount+staticList[n][act2*env.N_actions+1]*env.discount));
          }
      }
  }
}

void Environment::findMax(const Environment& env){
    //find optimal policy of each row of parameters
  for (int n=0; n< env.N_trials; ++n){
      float val = values[n][0];
      for (int index_n = 1; index_n < env.N_actions*env.N_actions; ++index_n){
          if (val <= values[n][index_n]){
            indexes[n] = index_n;
            val = values[n][index_n];
          }
      }
  }
}

void Environment::meanParsFunction(const Environment& env){
    for (int n=0; n< env.N_trials; ++n){
        for(int paramIndex = 0; paramIndex<env.N_actions*env.N_states; ++paramIndex){
            meanPars[indexes[n]][paramIndex] += staticList[n][paramIndex];
        }
        priors[indexes[n]] += 1;
    }

    //divide meanPars by number of observations to get average
    for(int index_policy=0; index_policy<env.N_actions*env.N_actions; ++index_policy){
        for(int paramIndex = 0; paramIndex<env.N_actions*env.N_states; ++paramIndex){
            meanPars[index_policy][paramIndex] /= priors[index_policy];
        }
    }

    //divide priors by N_trials
    for(int index_policy=0; index_policy<env.N_actions*env.N_actions; ++index_policy){
        priors[index_policy] /= env.N_trials;
    }
}