% Načtení IRIS Toolboxu
iris.startup;

% Načtení modelu
m = Model('g3p_model.mod');

% Nastavení parametrů
m.beta = 0.99;    % Diskontní faktor
m.kappa = 0.1;    % Citlivost inflace
m.rho_m = 0.6;    % Setrvačnost sazby
m.psi = 1.5;      % Reakce na inflaci
m.rho_oil = 0.2;  % Váha ropy
m.a11 = 0.8;      % Setrvačnost zahraničního HDP
m.a12 = 0.1;      % Citlivost na reálnou sazbu
m.a13 = 0.2;      % Citlivost na kurz
m.a91 = 0.7;      % Setrvačnost exportu
m.a92 = 0.5;      % Citlivost na zahraniční HDP
m.a93 = 0.3;      % Citlivost na kurz

% Řešení modelu
m = solve(m);

% Nastavení simulace (20 období)
d = zerodb(m, 1:20);

% Negativní poptávkový šok (pokles spotřeby o 1 %)
d.eps_c(1) = -0.01;

% Simulace IRF
s = simulate(m, d, 1:20, 'deviation', true);

% Vizualizace
figure;
subplot(2, 2, 1); plot(s.y); title('HDP (y)');
subplot(2, 2, 2); plot(s.pi); title('Inflace (pi)');
subplot(2, 2, 3); plot(s.i); title('Úroková sazba (i)');
subplot(2, 2, 4); plot(s.x); title('Export (x)');
sgtitle('IRF: Negativní poptávkový šok');% Načtení IRIS Toolboxu
iris.startup;

% Načtení modelu
m = Model('g3p_model.mod');

% Nastavení parametrů
m.beta = 0.99;    % Diskontní faktor
m.kappa = 0.1;    % Citlivost inflace
m.rho_m = 0.6;    % Setrvačnost sazby
m.psi = 1.5;      % Reakce na inflaci
m.rho_oil = 0.2;  % Váha ropy
m.a11 = 0.8;      % Setrvačnost zahraničního HDP
m.a12 = 0.1;      % Citlivost na reálnou sazbu
m.a13 = 0.2;      % Citlivost na kurz
m.a91 = 0.7;      % Setrvačnost exportu
m.a92 = 0.5;      % Citlivost na zahraniční HDP
m.a93 = 0.3;      % Citlivost na kurz

% Řešení modelu
m = solve(m);

% Nastavení simulace (20 období)
d = zerodb(m, 1:20);

% Negativní poptávkový šok (pokles spotřeby o 1 %)
d.eps_c(1) = -0.01;

% Simulace IRF
s = simulate(m, d, 1:20, 'deviation', true);

% Vizualizace
figure;
subplot(2, 2, 1); plot(s.y); title('HDP (y)');
subplot(2, 2, 2); plot(s.pi); title('Inflace (pi)');
subplot(2, 2, 3); plot(s.i); title('Úroková sazba (i)');
subplot(2, 2, 4); plot(s.x); title('Export (x)');
sgtitle('IRF: Negativní poptávkový šok');