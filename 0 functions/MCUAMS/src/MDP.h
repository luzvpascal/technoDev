#ifndef MDP_H
#define MDP_H

#include <vector>

class MDP {
public:
    MDP(int n_states, int n_actions, float discount);

    // Declare the copy constructor
    MDP(const MDP& other);

    // Declare the destructor
    ~MDP();

    // Getters for MDP attributes
    int getNStates() const;
    int getNActions() const;
    float getDiscount() const;

    // Getters and setters for transition and reward attributes
    float getTransition(int state1, int state2, int action) const;
    void setTransition(int state1, int state2, int action, float value);
    float getReward(int state, int action) const;
    void setReward(int state, int action, float value);

    float valueFunction(int init_state, float p1, float p2, float Rlow, float Rhigh) const;
    void setVectorValues(int act1, int act2);
    std::vector<float> getVectorValues() const;

private:
    int N_states; // Number of states
    int N_actions; // Number of actions
    float discount; // Discount factor
    std::vector<std::vector<std::vector<float>>> transition; // Transition probabilities
    std::vector<std::vector<float>> reward; // Reward values
    std::vector<float> vectorValues; // vector of values
};

#endif
