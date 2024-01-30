#include "MDP.h"

// Constructor
MDP::MDP(int n_states, int n_actions, float discount)
        : N_states(n_states), N_actions(n_actions), discount(discount) {
    // Initialize the transition probabilities with zeros
    transition.resize(N_states, std::vector<std::vector<float>>(N_states, std::vector<float>(N_actions, 0.0)));
    // Initialize the reward values with zeros
    reward.resize(N_states, std::vector<float>(N_actions, 0.0));

    vectorValues.resize(N_actions*N_actions, 0.0);
}

// Copy constructor implementation
MDP::MDP(const MDP& other)
        : N_states(other.N_states), N_actions(other.N_actions), discount(other.discount) {
    // Copy the transition probabilities
    transition = other.transition;

    // Copy the reward values
    reward = other.reward;

    // Copy the vectorValues values
    vectorValues = other.vectorValues;
}

// Destructor implementation
MDP::~MDP() {
    // The destructor can be left empty as there is no dynamic memory to release explicitly
}

int MDP::getNStates() const {
    return N_states;
}

int MDP::getNActions() const {
    return N_actions;
}

float MDP::getDiscount() const {
    return discount;
}

float MDP::getTransition(int state1, int state2, int action) const {
    if (state1 >= 0 && state1 < N_states && state2 >= 0 && state2 < N_states && action >= 0 && action < N_actions) {
        return transition[state1][state2][action];
    } else {
        // Handle invalid indices
        return 0.0;
    }
}

void MDP::setTransition(int state1, int state2, int action, float value) {
    if (state1 >= 0 && state1 < N_states && state2 >= 0 && state2 < N_states && action >= 0 && action < N_actions) {
        transition[state1][state2][action] = value;
    } else {
        // Handle invalid indices
    }
}

float MDP::getReward(int state, int action) const {
    if (state >= 0 && state < N_states && action >= 0 && action < N_actions) {
        return reward[state][action];
    } else {
        // Handle invalid indices
        return 0.0;
    }
}

void MDP::setReward(int state, int action, float value) {
    if (state >= 0 && state < N_states && action >= 0 && action < N_actions) {
        reward[state][action] = value;
    } else {
        // Handle invalid indices
    }
}

float MDP::valueFunction(int init_state, float p1, float p2, float Rlow, float Rhigh) const {
    if (init_state==0){
        return((Rlow*(1-discount+discount*p2) + Rhigh*(discount-discount*p1))/((1-discount)*(1-p1*discount+p2*discount)));
    } else {
        return((Rhigh*(1-p1*discount)+Rlow*p2*discount)/((1-discount)*(1-p1*discount+p2*discount)));
    }
}

void MDP::setVectorValues(int act1, int act2){
        vectorValues[act1*N_actions+act2] = valueFunction(0, getTransition(0,0,act1),
                                                          getTransition(1,0,act2),
                                                          getReward(0,act1),
                                                          getReward(1,act2));
}

std::vector<float> MDP::getVectorValues() const {
    return vectorValues;
}