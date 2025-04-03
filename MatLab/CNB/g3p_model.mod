!variables
    y, c, i, pi, s, x, z, y_star, pi_star, i_star

!shocks
    eps_c, eps_y_star, eps_pi, eps_i

!parameters
    beta, kappa, rho_m, psi, phi, rho_s, a11, a12, a13, a91, a92, a93

!steady
    y = 0;
    c = 0;
    i = 0;
    pi = 0;
    s = 0;
    x = 0;
    z = 0;
    y_star = 0;
    pi_star = 0;
    i_star = 0;

!equations
    % IS křivka (domácí HDP)
    y = 0.8*y{-1} + 0.2*c - 0.1*(i{-1} - pi) + 0.3*x + eps_c;

    % Eulerova rovnice (spotřeba)
    c = 0.5*c{+1} + 0.5*c{-1} - 0.2*(i - pi{+1});

    % Phillipsova křivka (inflace)
    pi = beta*pi{+1} + kappa*y;

    % Taylorovo pravidlo
    i = rho_m*i{-1} + (1 - rho_m)*(psi*pi + phi*y);

    % Směnný kurz (UIP)
    s = s{+1} + rho_s*(i_star - i);

    % Reálný směnný kurz
    z = s + pi_star - pi;

    % Exportní poptávka
    x = a91*x{-1} + a92*y_star + a93*z;

    % Zahraniční HDP
    y_star = a11*y_star{-1} - a12*(i_star{-1} - pi_star) + a13*z{-1} + eps_y_star;

    % Zahraniční inflace
    pi_star = 0.5*pi_star{-1} + 0.2*y_star{-1} + eps_pi;

    % Zahraniční úroková sazba
    i_star = 0.7*i_star{-1} + 0.15*pi_star{-1} + 0.1*y_star{-1} + eps_i;