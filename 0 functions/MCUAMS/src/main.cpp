#include <iostream>
#include <fstream>
#include "Environment.h"

int main(int argc, char* argv[]) {
    if (argc != 6) {
        std::cerr << "Usage: " << argv[0] << " <csvFilename> <outputMeanParsFileName> <outputPriorsFileName> <plowlow> <phighlow>" << std::endl;
        return 1;
    }

    // The first argument (argv[1]) is the CSV file name
    std::string csvFileName = argv[1];

    // The second argument (argv[2]) is the output file name
    std::string outputMeanParsFileName = argv[2];
    std::string outputPriorsFileName = argv[3];
    float plowlow = std::stof(argv[4]);
    float phighlow = std::stof(argv[5]);

    // Create an Environment object initialized with "reward_test.csv"
    float discount = 0.9;
    int n_trials = 100000;
    Environment env(csvFileName, discount, n_trials);

    // Check if the CSV file was successfully loaded
    if (env.getNStates() > 0 && env.getNActions() > 0) {
        //std::cout << "Environment created with " << env.getNStates() << " states and " << env.getNActions() << " actions." << std::endl;
        for(int n=0; n<env.getNTrials();++n){
            env.staticList[n][0] = plowlow;
            env.staticList[n][1] = phighlow;
        }

        Environment::computeValues(env);

        Environment::findMax(env);

        Environment::meanParsFunction(env);

        std::ofstream outfile(outputMeanParsFileName);

        if (outfile.is_open()) {
            outfile << "opt, p1, p2, p3, p4"<<std::endl;
            for(int n=0; n<env.getNActions()*env.getNActions();++n){
                if (env.priors[n] >0){
                    outfile << n+1 ;
                    for(int m=0; m<env.getNStates()*env.getNActions();++m){
                        outfile << ", " << env.meanPars[n][m];
                    }
                    outfile << std::endl;
                }
            }
            outfile.close();
        } else {
            std::cerr << "Unable to open the file." << std::endl;
        }

        std::ofstream outfile2(outputPriorsFileName);

        if (outfile2.is_open()) {
            outfile2 << "opt, freq" << std::endl;
            for (int n = 0; n < env.getNActions() * env.getNActions(); ++n) {
                if (env.priors[n] >0) {
                    outfile2 << n + 1 << ", " << env.priors[n] << std::endl;
                }
            }
            outfile2.close();
        } else {
            std::cerr << "Unable to open the file." << std::endl;
        }
    } else {
        std::cerr << "Failed to create the environment. Check the CSV file." << std::endl;
    }

    return 0;
}

//#include <iostream>
//#include "MDP.h"
//int main(){
//    int n_states = 2;
//    int n_actions = 3;
//    float discount = 0.9;
//
//    MDP mdp(n_states, n_actions, discount);
//
//    for (int s=0; s<mdp.getNStates(); ++s){
//        for (int s2=0; s2<mdp.getNStates(); ++s2){
//            for (int a=0; a<mdp.getNActions(); ++a){
//                mdp.setTransition(s, s2, a, 0.5);
//            }
//        }
//    }
//
//    for (int s=0; s<mdp.getNStates(); ++s){
//        for (int a=0; a<mdp.getNActions(); ++a){
//            mdp.setReward(s, a, 0.5+a+s);
//        }
//    }
//    mdp.setVectorValues(0,0);
//    auto vectortest = mdp.getVectorValues();
//    for (int a1=0; a1<mdp.getNActions(); ++a1){
//        for (int a2=0; a2<mdp.getNActions(); ++a2){
//            std::cout << vectortest[a1*mdp.getNActions()+a2] <<  std::endl;
//        }
//    }
//    return 0;
//}