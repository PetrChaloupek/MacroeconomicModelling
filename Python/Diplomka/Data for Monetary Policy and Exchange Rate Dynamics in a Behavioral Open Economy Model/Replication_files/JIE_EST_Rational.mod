// Estimation of Open Economy Behavioral New Keynesian Model
//
// by Marcin Kolasa, Sahil Ravgotra and Pawel Zabczyk

//We consider Canada as the SOE and the US as the (closed) rest of the world, with 3+3+1 shocks and 3+3+1 observables. 

//---------------------------------------------------------//
//                  Endogeneous Variables                  //
//---------------------------------------------------------//
var 
//---------------------------------------------------------//
//                     Domestic Economy                    //
//---------------------------------------------------------//
    c                                           ${\hat{c}_t}$                     (long_name='DOMESTIC AGGREGATE CONSUMPTION')                      // 1
    c_F                                         ${\hat{c}_{F,t}}$                 (long_name='DOMESTIC AGGREGATE CONSUMPTION OF FOREIGN PRODUCE')   // 2
    c_H                                         ${\hat{c}_{H,t}}$                 (long_name='DOMESTIC AGGREGATE CONSUMPTION OF DOMESTIC PRODUCE')  // 3
    g                                           ${g_t}$                           (long_name='DOMESTIC PREFERENCE SHOCK')                           // 4
    gamma_F                                     ${\hat{\Gamma}_{F,t}}$            (long_name='DOMESTIC RELATIVE PRICE OF IMPORTS')                  // 5
    gamma_H                                     ${\hat{\Gamma}_{H,t}}$            (long_name='DOMESTIC RELATIVE PRICE OF HOME GOODS')               // 6
    i                                           ${\hat{i}_t}$                     (long_name='DOMESTIC NOMINAL INTEREST RATE')                      // 7
    mc                                          ${\hat{mc}_t}$                    (long_name='DOMESTIC REAL MARGINAL COST')                         // 8
    n                                           ${\hat{n}_t}$                     (long_name='DOMESTIC LABOUR DEMAND')                              // 9
    nu                                          ${\nu_t}$                         (long_name='DOMESTIC MONETARY POLICY DISTURBANCE')                //10
    pi                                          ${\hat{\pi}_t}$                   (long_name='DOMESTIC CPI INFLATION')                              //11
    pi_F                                        ${\hat{\pi}_{F,t}}$               (long_name='DOMESTIC IMPORT PRICE INFLATION')                     //12
    pi_H                                        ${\hat{\pi}_{H,t}}$               (long_name='DOMESTIC PPI INFLATION')                              //13
    w                                           ${\hat{w}_t}$                     (long_name='DOMESTIC WAGE')                                       //14
    xi                                          ${\xi_t}$                         (long_name='DOMESTIC COST-PUSH SHOCK')                            //15
    y                                           ${\hat{y}_t}$                     (long_name='DOMESTIC AGGREGATE INCOME')                           //16
    dy                                          ${dy_t}$                          (long_name='DOMESTIC OBSERVED OUTPUT')                            //17
    pinfobs                                     ${pinfobs_t}$                     (long_name='DOMESTIC OBSERVED INFLATION')                         //18
    robs                                        ${robs_t}$                        (long_name='DOMESTIC OBSERVED RATE')                              //19
//---------------------------------------------------------//
//                     Foreign Economy                     //
//---------------------------------------------------------//
    c_star                                      ${\hat{c}_t^*}$                   (long_name='FOREIGN AGGREGATE CONSUMPTION')                       //20
    c_F_star                                    ${\hat{c}_{F,t}^*}$               (long_name='FOREIGN AGGREGATE CONSUMPTION OF FOREIGN PRODUCE')    //21
    c_H_star                                    ${\hat{c}_{H,t}^*}$               (long_name='FOREIGN AGGREGATE CONSUMPTION OF DOMESTIC PRODUCE')   //22   
    g_star                                      ${g_t^*}$                         (long_name='FOREIGN PREFERENCE SHOCK')                            //23                             
    gamma_F_star                                ${\hat{\Gamma}_{H,t}^*}$          (long_name='FOREIGN RELATIVE PRICE OF IMPORTS')                   //24                    
    gamma_H_star                                ${\hat{\Gamma}_{H,t}^*}$          (long_name='FOREIGN RELATIVE PRICE OF HOME GOODS')                //25               
    i_star                                      ${\hat{i}_t^*}$                   (long_name='FOREIGN NOMINAL INTEREST RATE')                       //26                        
    mc_star                                     ${\hat{mc}_t^*}$                  (long_name='FOREIGN REAL MARGINAL COST')                          //27                           
    n_star                                      ${\hat{n}_t^*}$                   (long_name='FOREIGN LABOUR DEMAND')                               //28                                
    nu_star                                     ${\nu_t^*}$                       (long_name='FOREIGN MONETARY POLICY DISTURBANCE')                 //29                 
    pi_star                                     ${\hat{\pi}_t^*}$                 (long_name='FOREIGN CPI INFLATION')                               //30                               
    pi_F_star                                   ${\hat{\pi}_{F,t}^*}$             (long_name='FOREIGN IMPORT PRICE INFLATION')                      //31                      
    pi_H_star                                   ${\hat{\pi}_{H,t}^*}$             (long_name='FOREIGN PPI INFLATION')                               //32                              
    w_star                                      ${\hat{w}_t^*}$                   (long_name='FOREIGN WAGE')                                        //33                                
    xi_star                                     ${\xi_t^*}$                       (long_name='FOREIGN COST-PUSH SHOCK')                             //34                             
    y_star                                      ${\hat{y}_t^*}$                   (long_name='FOREIGN AGGREGATE INCOME')                            //35 
    dy_star                                     ${dy_t^*}$                        (long_name='FOREIGN OBSERVED OUTPUT')                             //36
    pinfobs_star                                ${pinfobs_t^*}$                   (long_name='FOREIGN OBSERVED INFLATION')                          //37
    robs_star                                   ${robs_t^*}$                      (long_name='FOREIGN OBSERVED INTEREST RATE')                      //38
//---------------------------------------------------------//
//           NFA and Exchange Rate Definitions             //
//---------------------------------------------------------//
//Note: b_star was predetermined in the derivations so timing lagged by one period to make it consistent with dynare conventions
    b_star                                      ${\hat{b}_t^*}$                   (long_name='NET FOREIGN ASSET POSITION')                          //39
    e                                           ${\hat{e}_t}$                     (long_name='NOMINAL EXCHANGE RATE')                               //40
    q                                           ${\hat{Q}_t}$                     (long_name='REAL EXCHANGE RATE')                                  //41
    rp                                          ${\varrho_t}$                     (long_name='RISK PREMIUM SHOCK')                                  //42
    de                                          ${\hat{de}_t}$                    (long_name='CHANGE IN NOMINAL EXCHANGE RATE')                     //43
    de_obs                                      ${de_t^{obs}}$                    (long_name='OBSERVED NOMINAL EXCHANGE RATE')                      //44
    omega                                       ${\Omega_t}$                      (long_name='RISK PREMIUM')                                        //45
    ede
    ;

//---------------------------------------------------------//
//                  Exogeneous Variables                   //
//---------------------------------------------------------//
varexo
//---------------------------------------------------------//
//                    Domestic Shocks                      //
//---------------------------------------------------------//
    eps_nu                                      ${\varepsilon_{\nu}}$             (long_name='Monetary Policy Shock')                               // 1
    eps_g                                       ${\varepsilon_{g}}$               (long_name='Preference Shock')                                    // 2
    eps_xi                                      ${\varepsilon_{\xi}}$             (long_name='Cost-push Shock')                                     // 3
//---------------------------------------------------------//
//                     Foreign Shocks                      //
//---------------------------------------------------------//
    eps_nu_star                                 ${\varepsilon_{\nu*}}$            (long_name='Foreign Monetary Policy Shock')                       // 4
    eps_g_star                                  ${\varepsilon_{g*}}$              (long_name='Foreign Preference Shock')                            // 5
    eps_xi_star                                 ${\varepsilon_{\xi*}}$            (long_name='Foreign Cost-push Shock')                             // 6
    eps_rp                                      ${\varepsilon_{\varrho}}$         (long_name='Risk-Premium Shock')                                  // 7
    ;

//---------------------------------------------------------//
//                       Parameters                        //
//---------------------------------------------------------//
parameters 
    alppha                                      ${\alpha}$                          (long_name='Openness')
    betta                                       ${\beta}$                           (long_name='Discount Factor')
    delta                                       ${\delta}$                          (long_name='Sensitivity of intermediation costs to the level of foreign debt')
    epsilon                                     ${\epsilon}$                        (long_name='IES among goods produced in a same country')
    eta                                         ${\eta}$                            (long_name='Elasticity of substitution between domestic and foreign goods')
    m                                           ${\bar{m}}$                         (long_name='Cognative Discounting')
    //m_star                                      ${\bar{m}^*}$                       (long_name='Cognative Discounting')
    mu                                          ${\mu}$                             (long_name='Gross product Markup')
    phi_pi                                      ${\phi_{\pi}}$                      (long_name='Inflation Feedback Taylor Rule')
    phi_pi_star                                 ${\phi_{\pi}^*}$                    (long_name='Foreign Inflation Feedback Taylor Rule')
    phi_y                                       ${\phi_{y}}$                        (long_name='Output Feedback Taylor Rule')
    phi_y_star                                  ${\phi_{y}^*}$                      (long_name='Foreign Output Feedback Taylor Rule')
    rho_g                                       ${\rho_g}$                          (long_name='Autocorrelation Preference Shock')
    rho_g_star                                  ${\rho_g^*}$                        (long_name='Autocorrelation Preference Shock')
    rho_i                                       ${\rho_{i}}$                        (long_name='Interest rate smoothing parameter')
    rho_i_star                                  ${\rho_{i}^*}$                      (long_name='Interest rate smoothing parameter')
    rho_nu                                      ${\rho_nu}$                         (long_name='Autocorrelation Monetary Shock')
    rho_nu_star                                 ${\rho_nu^*}$                       (long_name='Autocorrelation Monetary Shock')
    rho_rp                                      ${\rho_\varrho}$                    (long_name='Autocorrelation Risk Premium Shock')
    rho_xi                                      ${\rho_xi}$                         (long_name='Autocorrelation Cost-push Shock')
    rho_xi_star                                 ${\rho_xi^*}$                       (long_name='Autocorrelation Cost-push Shock')
    siggma                                      ${\sigma}$                          (long_name='Effective Intertemporal elasticity of substitution')
    theta                                       ${\theta}$                          (long_name='Calvo Probability of not changing price')
    varphi                                      ${\phi}$                            (long_name='Inverse Frisch Elasticity of Labor Supply')
    zetta                                       ${\zetta}$                          (long_name='Country Size')
//---------------------------------------------------------//
//       Constants in the Observable Equations             //
//---------------------------------------------------------//
    rA                                          ${r^A}$                             (long_name='Steady State Domestic Real Rate')
    piA                                         ${\pi^A}$                           (long_name='Steady State Domestic Inflation')
    yA                                          ${y^A}$                             (long_name='Steady State Domestic Output')
    rA_star                                     ${r^{A*}}$                          (long_name='Steady State Foreign Real Rate')
    piA_star                                    ${\pi^{A*}}$                        (long_name='Steady State Foreign Inflation')
    yA_star                                     ${y^{A*}}$                          (long_name='Steady State Foreign Output')
    deA                                         ${Q^{A*}}$                          (long_name='Steady State Exchange Rate')

//---------------------------------------------------------//
//                      Steady State Values                //
//---------------------------------------------------------//
    B_star_Y_ss 
    Y_Y_star_ss
    YC_star_ss
    YC_ss
//---------------------------------------------------------//
//                     Switches                            //
//---------------------------------------------------------//
    s_1                                         // Set to 0 zero to switch off extra terms in IS curve (default 1)
    s_2                                         // Set to 0 to switch off discounting of inflation in real rate definition (default 1)
    s_3                                         // Set to 0 to impose closed economy version of the model (default 1)
    s_5                                         // Set to 0 to switch off all shocks other than domestic monetary policy shocks (default 1)
;





//---------------------------------------------------------//
//    Assing values to parameters from params structure    //
//---------------------------------------------------------//
names  = fieldnames(params);
values = struct2cell(params);
for i = 1:numel(names)
    set_param_value(names{i},values{i});
end

options_.TeX=1;

//---------------------------------------------------------//
//                          Model                          //
//---------------------------------------------------------//
model(linear);

#lambda           = (1-theta)*(1-betta*theta)/theta;
#alppha_star      = zetta/(1-zetta)*YC_star_ss*Y_Y_star_ss*(1-(1-alppha)/YC_ss);
#m_star=m;

//---------------------------------------------------------//
//                     Domestic Block                      //
//---------------------------------------------------------//
//1. IS Curve
c= m*c(+1)- (1/(siggma*YC_ss))*(i- (s_2*m+(1-s_2))*pi(+1)+m*g(+1)-g)
        +s_1*((1-m)*(1-betta)*(1+(siggma/(mu*varphi))*YC_ss)^(-1)*(b_star-B_star_Y_ss*q));

//2. Optimal Labor Supply
w= siggma*(YC_ss)*c+ varphi*n;

//3. Consumption Basket
YC_ss*c= (1-alppha)*c_H+ alppha*c_F;

//4. Demand Function for Home Goods
c_H= YC_ss*c- eta*gamma_H;


//5. Demand Function for Foreign Goods 
c_F= YC_ss*c- eta*gamma_F;


//6. Marginal Cost
mc= w-gamma_H;

//7. Aggregate Production Function
y=n;

//8. Phillips Curve
pi_H= betta*m*pi_H(+1)+ lambda*mc+xi;

//9. import Price Inflation
pi_F= gamma_F- gamma_F(-1)+ pi;

//10. CPI Inflation
pi= (1-alppha)*pi_H+ alppha*pi_F;

//11. Monetary Policy Rule
i=rho_i*i(-1) + (1-rho_i)*(phi_pi*pi+phi_y*y)+nu; 

//12. Resource Constraint   
y=(1-alppha)*c_H+ alppha*c_H_star;

//13. Law of One Price
gamma_H= gamma_H_star+ q;

//14. Preference Shock
g= rho_g*g(-1) + s_5*eps_g;

//15. Cost-push Shock
xi= rho_xi*xi(-1) + s_5*eps_xi;

//16. Monetary Policy Shock
nu=rho_nu*nu(-1)+eps_nu;

//---------------------------------------------------------//
//                     Foreign Block                       //
//---------------------------------------------------------//

//17. Foreign IS Curve
c_star= m_star*c_star(+1)- (1/(siggma*YC_star_ss))*(i_star- (s_2*m_star+(1-s_2))*pi_star(+1)+m_star*g_star(+1)-g_star);
    
//18. Foreign Optimal Labor Supply
w_star= siggma*(YC_star_ss)*c_star+ varphi*n_star;

//19. Foreign Consumption Basket
c_star= c_F_star;

//20. Foreign Demand Function for Home Goods
c_H_star= c_star- eta*gamma_H_star;

//21. Foreign Demand Function for Foreign Goods
gamma_F_star=0;

//22. Foreign Marginal Cost
mc_star= w_star-gamma_F_star;

//23. Foreign Aggregate Production Function
y_star=n_star;

//24. Foreign Phillips Curve
pi_F_star= betta*m_star*pi_F_star(+1)+ lambda*mc_star+xi_star;

//25. Foreign import Price Inflation
pi_H_star= gamma_H_star- gamma_H_star(-1)+ pi_star;

//26. Foreign CPI Inflation
pi_star= pi_F_star;

//27. Monetary Policy Rule
i_star=rho_i_star*i_star(-1) + (1-rho_i_star)*(phi_pi_star*pi_star+phi_y_star*y_star)+nu_star; 

//28. Foreign Resource Constraint   
y_star=c_F_star;

//29. Law of One Price
gamma_F= gamma_F_star+ q;

//30. Foreign Preference Shock
g_star = rho_g_star*g_star(-1) + s_5*eps_g_star;

//31. Foreign Cost-push Shock
xi_star = rho_xi_star*xi_star(-1) + s_5*eps_xi_star;

//32. Foreign Monetary Policy Shock
nu_star=rho_nu_star*nu_star(-1)+s_5*eps_nu_star;

//---------------------------------------------------------//
//                NFA and Exchange Rates                   //
//---------------------------------------------------------//
//33. Nominal Exchange Rate
q- q(-1)= e- e(-1)+ pi_star-pi;

//34. UIP Condition (or q = 0 in closed economy version)
q= s_3*(i_star-omega-m*(pi_star(+1)-q(+1))-(i-(s_2*m+(1-s_2))*pi(+1)));

//35. NFA Law of Motion (or b_star = 0 in closed economy version)
b_star= s_3*(B_star_Y_ss*(i_star+ (1/betta)*(q-q(-1)-pi_star))+ (1/betta)*(b_star(-1)+y-c));


//---------------------------------------------------------//
//             Observables                                 //
//---------------------------------------------------------//
//36. Domestic Output
dy= yA + y - y(-1);

//37. Domestic Inflation
pinfobs= piA + pi_H;

//38. Domestic Real Rate
robs= piA + rA + i;

//39. Foreign Output
dy_star= yA_star + y_star - y_star(-1);

//40. Foreign Inflation
pinfobs_star= piA_star + pi_star;

//41. Foreign Real Rate
robs_star= piA_star + rA_star + i_star;

//42. Exchange Rate
de_obs= de + piA-piA_star + deA;

//---------------------------------------------------------//
//             Extra                                       //
//---------------------------------------------------------//

//43. Risk Premium Shock
rp= rho_rp*rp(-1) + eps_rp;

//44. Change in Nominal Exchange Rate
de= (q-q(-1)+pi-pi_star);

//43. Risk Premium
omega= delta*b_star(-1) + rp;

//44. ER Expectations
ede=m*q(+1) - q + m*pi(+1) - m*pi_star(+1);

end;


//---------------------------------------------------------//
//                Shock Standard Deviations                //
//---------------------------------------------------------//

shocks;
//---------------------------------------------------------//
//                     Domestic Shocks                     //
//---------------------------------------------------------//                 
var eps_nu;             stderr 0.25;              
var eps_g;              stderr 0.25;
var eps_xi;             stderr 0.25;
//--------------------------=------------------------------//
//                     Foreign Shocks                      //
//---------------------------------------------------------//               
var eps_nu_star;        stderr 0.25;              
var eps_g_star;         stderr 0.25;
var eps_xi_star;        stderr 0.25;
var eps_rp;             stderr 0.25;   
end;


check;
steady;


//---------------------------------------------------------//
//                 Bayesian Estimation                     //
//---------------------------------------------------------//

//---------------------------------------------------------//
//                 Priors                                  //
//---------------------------------------------------------//
estimated_params;
    //m,                                        BETA_PDF,              0.80,  0.10;                        // Home Cognative Discounting
    phi_pi,                                   NORMAL_PDF,            1.5,   0.50;                        // Home Inflation Feedback Taylor Rule
    phi_y,                                    NORMAL_PDF,            0.125, 0.13;                        // Home Output Feedback Taylor Rule
    phi_pi_star,                              NORMAL_PDF,            1.5,   0.50;                        // Foreign Inflation Feedback Taylor Rule
    phi_y_star,                               NORMAL_PDF,            0.125, 0.13;                       // Foreign Output Feedback Taylor Rule
    rho_rp,                                   BETA_PDF,              0.70,  0.10;                        // Autocorrelation Risk Premium Shock
    rho_g,                                    BETA_PDF,              0.70,  0.10;                        // Autocorrelation Preference Shock, (IN JP, MEAN IS 0.8 FOR PREF, TECH AND RISK PREMIUM SHOCK)
    rho_g_star,                               BETA_PDF,              0.70,  0.10;                        // F Autocorrelation Preference Shock
    rho_i,                                    BETA_PDF,              0.90,  0.05;                        // Interest rate smoothing parameter
    rho_i_star,                               BETA_PDF,              0.90,  0.05;                        // F Interest rate smoothing parameter
    rho_xi,                                   BETA_PDF,              0.70,  0.10;                        // Autocorrelation Cost-push Shock
    rho_xi_star,                              BETA_PDF,              0.70,  0.10;                        // Autocorrelation Cost-push Shock
    siggma,                                   NORMAL_PDF,             1.0, 0.20;
    theta,                                    BETA_PDF,              0.875,  0.05;                       // Calvo Probability of not changing price 
    varphi,                                   GAMMA_PDF,             3,   0.25;                         // Inverse Frisch Elasticity of Labor Supply, JP(2010)
    rA,                                       NORMAL_PDF,            0.5,   0.25;                        // Steady State Domestic Real Rate
    piA,                                      NORMAL_PDF,            1,     0.25;                        // Steady State Domestic Inflation
    yA,                                       NORMAL_PDF,            0.5,   0.25;                        // Steady State Domestic Output
    rA_star,                                  NORMAL_PDF,            0.5,   0.25;                        // Steady State Foreign Real Rate
    piA_star,                                 NORMAL_PDF,            1,     0.25;                        // Steady State Foreign Inflation
    yA_star,                                  NORMAL_PDF,            0.50,  0.25;                        // Steady State Foreign Output
    deA,                                      NORMAL_PDF,            0.50,  0.25;                         // Steady State Real Exchange Rate
    stderr eps_nu,                            INV_GAMMA_PDF,         0.25,  inf;                        // SD H Monetary Policy
    stderr eps_g,                             INV_GAMMA_PDF,         0.25,  inf;                        // SD H Preference
    stderr eps_xi,                            INV_GAMMA_PDF,         0.25,  inf;                        // SD H Cost-push
    stderr eps_nu_star,                      INV_GAMMA_PDF,         0.25,  inf;                        // SD F Monetary Policy
    stderr eps_g_star,                       INV_GAMMA_PDF,         0.25,  inf;                        // SD F Preference
    stderr eps_xi_star,                       INV_GAMMA_PDF,         0.25,  inf;                        // SD F Cost-push
    stderr eps_rp,                            INV_GAMMA_PDF,         0.25,  inf;                        // SD RP shock
    eta,                                     GAMMA_PDF,             1,  0.05;
end;


steady_state_model;
dy                  = yA;
pinfobs             = piA;
robs                = piA + rA;
dy_star             = yA_star;
pinfobs_star        = piA_star;
robs_star           = piA_star + rA_star; 
de_obs              = piA-piA_star + deA; 
end;


varobs dy pinfobs robs dy_star pinfobs_star robs_star de_obs;


estimation(
    datafile='DATA_KRZ_7207',
    mode_compute=6,
    mh_replic=250000,
    mh_nblocks=1,
    mh_jscale=0.3,
    mh_drop=0.6,
    lik_init=2,
    presample=4,
    prefilter=0, 
    kalman_algo=1
    ) 
dy pinfobs robs dy_star pinfobs_star robs_star de_obs;


stoch_simul(order=1, irf=40,ar=10, nograph);


