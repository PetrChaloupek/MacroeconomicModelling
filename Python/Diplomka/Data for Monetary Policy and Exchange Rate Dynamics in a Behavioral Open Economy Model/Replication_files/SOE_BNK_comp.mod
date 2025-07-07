// Open Economy Behavioral New Keynesian Model with complete markets
//
// by Marcin Kolasa, Sahil Ravgotra and Pawel Zabczyk


//---------------------------------------------------------//
//                  Endogeneous Variables                  //
//---------------------------------------------------------//
var 
//---------------------------------------------------------//
//                     Domestic Economy                    //
//---------------------------------------------------------//
    a                                           ${\hat{a}_t}$                     (long_name='DOMESTIC TECHNOLOGICAL PROGRESS')                     // 1
    c                                           ${\hat{c}_t}$                     (long_name='DOMESTIC AGGREGATE CONSUMPTION')                      // 2
    c_F                                         ${\hat{c}_{F,t}}$                 (long_name='DOMESTIC AGGREGATE CONSUMPTION OF FOREIGN PRODUCE')   // 3
    c_H                                         ${\hat{c}_{H,t}}$                 (long_name='DOMESTIC AGGREGATE CONSUMPTION OF DOMESTIC PRODUCE')  // 4
    g                                           ${g_t}$                           (long_name='DOMESTIC PREFERENCE SHOCK')                           // 5
    gamma_F                                     ${\hat{\Gamma}_{F,t}}$            (long_name='DOMESTIC RELATIVE PRICE OF IMPORTS')                  // 6
    gamma_H                                     ${\hat{\Gamma}_{H,t}}$            (long_name='DOMESTIC RELATIVE PRICE OF HOME GOODS')               // 7
    i                                           ${\hat{i}_t}$                     (long_name='DOMESTIC NOMINAL INTEREST RATE')                      // 8
    mc                                          ${\hat{mc}_t}$                    (long_name='DOMESTIC REAL MARGINAL COST')                         // 9
    n                                           ${\hat{n}_t}$                     (long_name='DOMESTIC LABOUR DEMAND')                              //10
    nu                                          ${\nu_t}$                         (long_name='DOMESTIC MONETARY POLICY DISTURBANCE')                //11
    pi                                          ${\hat{\pi}_t}$                   (long_name='DOMESTIC CPI INFLATION')                              //12
    pi_F                                        ${\hat{\pi}_{F,t}}$               (long_name='DOMESTIC IMPORT PRICE INFLATION')                     //13
    pi_H                                        ${\hat{\pi}_{H,t}}$               (long_name='DOMESTIC PPI INFLATION')                              //14
    w                                           ${\hat{w}_t}$                     (long_name='DOMESTIC WAGE')                                       //15
    xi                                          ${\xi_t}$                         (long_name='DOMESTIC COST-PUSH SHOCK')                            //16
    y                                           ${\hat{y}_t}$                     (long_name='DOMESTIC AGGREGATE INCOME')                           //17
//---------------------------------------------------------//
//                     Foreign Economy                     //
//---------------------------------------------------------//
    a_star                                      ${\hat{a}_t^*}$                   (long_name='FOREIGN TECHNOLOGICAL PROGRESS')                      //18
    c_star                                      ${\hat{c}_t^*}$                   (long_name='FOREIGN AGGREGATE CONSUMPTION')                       //19
    c_F_star                                    ${\hat{c}_{F,t}^*}$               (long_name='FOREIGN AGGREGATE CONSUMPTION OF FOREIGN PRODUCE')    //20
    c_H_star                                    ${\hat{c}_{H,t}^*}$               (long_name='FOREIGN AGGREGATE CONSUMPTION OF DOMESTIC PRODUCE')   //21   
    g_star                                      ${g_t^*}$                         (long_name='FOREIGN PREFERENCE SHOCK')                            //22                             
    gamma_F_star                                ${\hat{\Gamma}_{H,t}^*}$          (long_name='FOREIGN RELATIVE PRICE OF IMPORTS')                   //23                    
    gamma_H_star                                ${\hat{\Gamma}_{H,t}^*}$          (long_name='FOREIGN RELATIVE PRICE OF HOME GOODS')                //24               
    i_star                                      ${\hat{i}_t^*}$                   (long_name='FOREIGN NOMINAL INTEREST RATE')                       //25                        
    mc_star                                     ${\hat{mc}_t^*}$                  (long_name='FOREIGN REAL MARGINAL COST')                          //26                           
    n_star                                      ${\hat{n}_t^*}$                   (long_name='FOREIGN LABOUR DEMAND')                               //27                                
    nu_star                                     ${\nu_t^*}$                       (long_name='FOREIGN MONETARY POLICY DISTURBANCE')                 //28                 
    pi_star                                     ${\hat{\pi}_t^*}$                 (long_name='FOREIGN CPI INFLATION')                               //29                               
    pi_F_star                                   ${\hat{\pi}_{F,t}^*}$             (long_name='FOREIGN IMPORT PRICE INFLATION')                      //30                      
    pi_H_star                                   ${\hat{\pi}_{H,t}^*}$             (long_name='FOREIGN PPI INFLATION')                               //31                              
    w_star                                      ${\hat{w}_t^*}$                   (long_name='FOREIGN WAGE')                                        //32                                
    xi_star                                     ${\xi_t^*}$                       (long_name='FOREIGN COST-PUSH SHOCK')                             //33                             
    y_star                                      ${\hat{y}_t^*}$                   (long_name='FOREIGN AGGREGATE INCOME')                            //34                            
    x                                           ${\hat{x}_t}$                     (long_name='Output Gap')
    y_bar                                       ${\bar{y}_t}$                     (long_name='Natural Rate of Output')
    r                                           ${\hat{r}_t}$                     (long_name='Real Interest Rate')
    p                                           ${\hat{p}_t}$                     (long_name='Price Level')
    p_H                                         ${\hat{p}_{H,t}}$                 (long_name='Producer Price Level')
//---------------------------------------------------------//
//           NFA and Exchange Rate Definitions             //
//---------------------------------------------------------//
    b_star                                      ${\hat{b}_t^*}$                   (long_name='NET FOREIGN ASSET POSITION')                          //35
    e                                           ${\hat{e}_t}$                     (long_name='NOMINAL EXCHANGE RATE')                               //36
    q                                           ${\hat{Q}_t}$                     (long_name='REAL EXCHANGE RATE')                                  //37
    int_diff
    nfa
//---------------------------------------------------------//
//           Fama and Engel Regression Coefficients        //
//---------------------------------------------------------//
    de                                          ${de_t}$                        (long_name='EXCHANGE RATE DIFFERENTIAL') 
    rho                                         ${\rho_t}$                      (long_name='EXCESS RETURN ON FOREIGN DEPOSIT')
    //This adds extra variables to produce Engel Regression Coeffieients (if j specified)
    @#ifdef j
        @#for counter in (0:j)
            id@{counter} 
        @#endfor
   @#endif
//---------------------------------------------------------//
//           News Shocks and Lower for Longer              //
//---------------------------------------------------------//
    //This adds extra variables for LFL and News Shock computations (optional)
    //To have this evaluate, invoke dynare with the -DT=XXX option where XXX denotes the horizon of interest. For example:
    //     dynare OE_BNK savemacro=OE_BNK_macro.mod -DT=80 
    //would create an 80 period shock term structure and save the macro-processed file in OE_BNK_macro.mod for inspection
    @#ifdef T
        @#for counter in (0:T)
            errffr@{counter}
        @#endfor
    @#endif
    //LFL in foreign economy (assumption only one used at any time)
    @#ifdef T_star
        @#for counter in (0:T_star)
            errffr@{counter}
        @#endfor
    @#endif
    ;

//---------------------------------------------------------//
//                  Exogeneous Variables                   //
//---------------------------------------------------------//
varexo
//---------------------------------------------------------//
//                    Domestic Shocks                      //
//---------------------------------------------------------//
    eps_a                                       ${\varepsilon_{a}}$               (long_name='Technology Shock')                                    // 1
    eps_nu                                      ${\varepsilon_{\nu}}$             (long_name='Monetary Policy Shock')                               // 2
    eps_g                                       ${\varepsilon_{g}}$               (long_name='Preference Shock')                                    // 3
    eps_xi                                      ${\varepsilon_{\xi}}$             (long_name='Cost-push Shock')                                     // 4
//---------------------------------------------------------//
//                     Foreign Shocks                      //
//---------------------------------------------------------//
    eps_a_star                                  ${\varepsilon_{a*}}$              (long_name='Foreign Technology Shock')                            // 5
    eps_nu_star                                 ${\varepsilon_{\nu*}}$            (long_name='Foreign Monetary Policy Shock')                       // 6
    eps_g_star                                  ${\varepsilon_{g*}}$              (long_name='Foreign Preference Shock')                            // 7
    eps_xi_star                                 ${\varepsilon_{\xi*}}$            (long_name='Foreign Cost-push Shock')                             // 8
//---------------------------------------------------------//
//           News Shocks and Lower for Longer              //
//---------------------------------------------------------//
    @#ifdef T
        @#for counter in (0:T)
            errshk@{counter}
        @#endfor
    @#endif
    //LFL in foreign economy (assumption only one used at any time)
    @#ifdef T_star
        @#for counter in (0:T_star)
            errshk@{counter}
        @#endfor
    @#endif
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
    m_star                                      ${\bar{m}^*}$                       (long_name='Cognative Discounting')
    mu                                          ${\mu}$                             (long_name='Gross product Markup')
    phi_pi                                      ${\phi_{\pi}}$                      (long_name='Inflation Feedback Taylor Rule')
    phi_pi_star
    phi_y                                       ${\phi_{y}}$                        (long_name='Output Feedback Taylor Rule')
    phi_y_star
    rho_a                                       ${\rho_a}$                          (long_name='Autocorrelation Technology Shock')
    rho_a_star                                  ${\rho_a^*}$                        (long_name='Autocorrelation Technology Shock')
    rho_g                                       ${\rho_g}$                          (long_name='Autocorrelation Preference Shock')
    rho_g_star                                  ${\rho_g^*}$                        (long_name='Autocorrelation Preference Shock')
    rho_i                                       ${\rho_{i}}$                        (long_name='Interest rate smoothing parameter')
    rho_i_star                                  ${\rho_{i}^*}$                      (long_name='Interest rate smoothing parameter')
    rho_nu                                      ${\rho_nu}$                         (long_name='Autocorrelation Monetary Shock')
    rho_nu_star                                 ${\rho_nu^*}$                       (long_name='Autocorrelation Monetary Shock')
    rho_xi                                      ${\rho_xi}$                         (long_name='Autocorrelation Cost-push Shock')
    rho_xi_star                                 ${\rho_xi^*}$                       (long_name='Autocorrelation Cost-push Shock')
    siggma                                      ${\sigma}$                          (long_name='Effective Intertemporal elasticity of substitution')
    theta                                       ${\theta}$                          (long_name='Calvo Probability of not changing price')
    varphi                                      ${\phi}$                            (long_name='Inverse Frisch Elasticity of Labor Supply')
    zetta                                       ${\zetta}$                          (long_name='Country Size')
//---------------------------------------------------------//
//       Composite Parameters and Steady State Values      //
//---------------------------------------------------------//
    alppha_star
    lambda
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
    s_4                                         // Set to 0 to apply news shocks (if specified) to real rate (default 1; apply to nominal one)
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
delta=0;


//---------------------------------------------------------//
//                          Model                          //
//---------------------------------------------------------//
model;

//---------------------------------------------------------//
//                     Domestic Block                      //
//---------------------------------------------------------//
//1. IS Curve
c= m*c(+1)- (1/(siggma*YC_ss))*(i- (s_2*m+(1-s_2))*pi(+1)+m*g(+1)-g)
        +s_1*((1-m)*(1-betta)*(1+(siggma/(mu*varphi))*YC_ss)^(-1)*(b_star(+1)-B_star_Y_ss*q));

//2. Optimal Labor Supply
w= siggma*(YC_ss)*c+ varphi*n;

//3. Consumption Basket
YC_ss*c= (1-alppha)*c_H+ alppha*c_F;

//4. Demand Function for Home Goods
c_H= YC_ss*c- eta*gamma_H;

//5. Demand Function for Foreign Goods
c_F= YC_ss*c- eta*gamma_F;

//6. Marginal Cost
mc= w-a-gamma_H;

//7. Aggregate Production Function
y=a+n;

//8. Phillips Curve
pi_H= betta*m*pi_H(+1)+ lambda*(mc+xi);

//9. import Price Inflation
pi_F= gamma_F- gamma_F(-1)+ pi;

//10. CPI Inflation
pi= (1-alppha)*pi_H+ alppha*pi_F;

//11. Monetary Policy Rule
@#ifdef T
    //Shock either the real interest rate (discounted or not) or the nominal interest rate
    i - (1-s_4)*((s_2*m+(1-s_2))*pi(+1))= s_4*(rho_i*i(-1) + (1-rho_i)*(phi_pi*pi+phi_y*y)+nu) + errffr0; 
@#else
    //Use standard monetary policy rule if no "news shocks present"
    i=rho_i*i(-1) + (1-rho_i)*(phi_pi*pi+phi_y*y)+nu; 
@#endif

//11.A Output Gap
x=y-y_bar;

//11.B Natural Output
a=((siggma+varphi)/(1+varphi))*y_bar;

//11.C Real Rate
r=i-m*pi(+1);

//11.D Price level
pi=p-p(-1);

//11.E Home produced good price level
pi_H = p_H - p_H(-1);

//12. Resource Constraint   
y=(1-alppha)*c_H+ alppha*c_H_star;

//13. Law of One Price
gamma_H= gamma_H_star+ q;

//14. Technology Shock
a= rho_a*a(-1) + s_5*eps_a;

//15. Preference Shock
g= rho_g*g(-1) + s_5*eps_g;

//16. Cost-push Shock
xi= rho_xi*xi(-1) + s_5*eps_xi;

//17. Monetary Policy Shock
nu=rho_nu*nu(-1)+eps_nu;

//---------------------------------------------------------//
//                     Foreign Block                       //
//---------------------------------------------------------//

//18. Foreign IS Curve
c_star= m_star*c_star(+1)- (1/(siggma*YC_star_ss))*(i_star- (s_2*m_star+(1-s_2))*pi_star(+1)+m_star*g_star(+1)-g_star);
    
//19. Foreign Optimal Labor Supply
w_star= siggma*(YC_star_ss)*c_star+ varphi*n_star;

//20. Foreign Consumption Basket
c_star= c_F_star;

//21. Foreign Demand Function for Home Goods
c_H_star= c_star- eta*gamma_H_star;

//22. Foreign Demand Function for Foreign Goods
gamma_F_star=0;

//23. Foreign Marginal Cost
mc_star= w_star-a_star-gamma_F_star;

//24. Foreign Aggregate Production Function
y_star=a_star+n_star;

//25. Foreign Phillips Curve
pi_F_star= betta*m_star*pi_F_star(+1)+ lambda*(mc_star+xi_star);

//26. Foreign import Price Inflation
pi_H_star= gamma_H_star- gamma_H_star(-1)+ pi_star;

//27. Foreign CPI Inflation
pi_star= pi_F_star;

//28. Monetary Policy Rule
@#ifdef T_star
     //Shock the nominal interest rate (only LFL considered here)
    i_star=rho_i_star*i_star(-1) + (1-rho_i_star)*(phi_pi_star*pi_star+phi_y_star*y_star)+nu_star + errffr0;
@#else
    //Use standard monetary policy rule if no "news shocks present"
    i_star=rho_i_star*i_star(-1) + (1-rho_i_star)*(phi_pi_star*pi_star+phi_y_star*y_star)+nu_star; 
@#endif

//29. Foreign Resource Constraint   
y_star=c_F_star;

//30. Law of One Price
gamma_F= gamma_F_star+ q;

//31. Foreign Technology Shock
a_star = rho_a_star*a_star(-1) + s_5*eps_a_star;

//32. Foreign Preference Shock
g_star = rho_g_star*g_star(-1) + s_5*eps_g_star;

//33. Foreign Cost-push Shock
xi_star = rho_xi_star*xi_star(-1) + s_5*eps_xi_star;

//34. Foreign Monetary Policy Shock
nu_star=rho_nu_star*nu_star(-1)+s_5*eps_nu_star;

//---------------------------------------------------------//
//                NFA and Exchange Rates                   //
//---------------------------------------------------------//
//35. Nominal Exchange Rate
q- q(-1)= e- e(-1)+ pi_star-pi;

//36. UIP Condition (or q = 0 in closed economy version)
//q= s_3*(i_star-delta*b_star(-1)-m*(pi_star(+1)-q(+1))-(i-(s_2*m+(1-s_2))*pi(+1)));

//36.A. Perfect Risk Sharing Condition
q= s_3*(siggma*(c-c_star));

//37. NFA Law of Motion (or b_star = 0 in closed economy version)
//b_star(+1)= s_3*(B_star_Y_ss*(i_star+ (1/betta)*(q-q(-1)-pi_star))+ (1/betta)*(b_star+y-c));
            s_3*(-b_star(+1)+ B_star_Y_ss*(i_star+ (1/betta)*(q-q(-1)-pi_star))+ (1/betta)*(b_star+y-c)) + (1-s_3)*b_star;

//37.A NFA
nfa=betta*b_star(+1);

//38. Interest rate differential
int_diff = i - i_star + delta*b_star(-1);

//---------------------------------------------------------//
//             Fama and Engel Regression                   //
//---------------------------------------------------------//
//39. Excess Return on Foreign Deposit
rho = de- (i(-1)-i_star(-1)+delta*b_star(-2));

//40. Exchange Rate Differential
de = e-e(-1);

//---------------------------------------------------------//
//              Interest Rate Differentials                //
//---------------------------------------------------------//
@#ifdef j
    @#for counter in (0:j)
        @#if counter>0
            id@{counter}=i(-@{counter})-i_star(-@{counter})+delta*b_star(-@{counter+1});
        @#else
            id@{counter} = i-i_star+delta*b_star(-@{counter+1});
        @#endif
    @#endfor
@#endif

//---------------------------------------------------------//
//           News Shocks and Lower for Longer              //
//---------------------------------------------------------//
@#ifdef T
    @#for counter in (0:T)
        @#if counter<T
            errffr@{counter}  = errffr@{counter+1}(-1)  + errshk@{counter};
        @#else
            errffr@{counter}  =                           errshk@{counter};
        @#endif
    @#endfor
@#endif

// Foreign economy equivalent
@#ifdef T_star
    @#for counter in (0:T_star)
        @#if counter<T_star
            errffr@{counter}  = errffr@{counter+1}(-1)  + errshk@{counter};
        @#else
            errffr@{counter}  =                           errshk@{counter};
        @#endif
    @#endfor
@#endif
end;

//---------------------------------------------------------//
//                Shock Standard Deviations                //
//---------------------------------------------------------//

shocks;
//---------------------------------------------------------//
//                     Domestic Shocks                     //
//---------------------------------------------------------//
var eps_a;              stderr 0.25;                    
var eps_nu;             stderr 0.25;              
var eps_g;              stderr 0.25;
var eps_xi;             stderr 0.25;
//--------------------------=------------------------------//
//                     Foreign Shocks                      //
//---------------------------------------------------------//
var eps_a_star;         stderr 0.25;                    
var eps_nu_star;        stderr 0.25;              
var eps_g_star;         stderr 0.25;
var eps_xi_star;        stderr 0.25;
//---------------------------------------------------------//
//            News Shocks and Lower for Longer             //
//---------------------------------------------------------//
@#ifdef T
    @#for counter in (0:T)
        var errshk@{counter}; stderr 0.25;
    @#endfor
@#endif
// Foreign economy equivalent
@#ifdef T_star
    @#for counter in (0:T_star)
        var errshk@{counter}; stderr 0.25;
    @#endfor
@#endif
end;

//---------------------------------------------------------//
//                 Simulation Instructions                 //
//---------------------------------------------------------//

//check;
//steady;

//This makes it possible to specify stoch_simul options and variables when launching dynare
//Specified as e.g., dynare OE_BNK '-DOptions="order=1, irf=100, noprint, nograph"' '-DVar_List="y pi i"'

@#ifdef Var_List
    @#ifdef Options
        stoch_simul(@{Options}) @{Var_List};
    @#else
        stoch_simul(order=1, irf=100, noprint, nograph) @{Var_List};
    @#endif
@#endif

//Because macroprocessor seemed not to like two nested ifdefs
@#ifndef Var_List
    @#ifdef Options
        stoch_simul(@{Options}) y pi i;
    @#else
        stoch_simul(order=1, irf=100, noprint, nograph) y pi i;
    @#endif
@#endif

irfinfo = oo_.irfs;

//This makes it possible to save IRFs under File_Name
//Specified as e.g., dynare OE_BNK '-DFile_Name="My_Results"'
@#ifdef File_Name
    save @{File_Name} irfinfo;
@#else
    save impulses_OE_BNK irfinfo;
@#endif
