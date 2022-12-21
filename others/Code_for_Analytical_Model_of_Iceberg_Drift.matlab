% Iceberg Model -----------------------------------------------------------
%
% This model accompanies the paper "An Analytical Model of Iceberg Drift"
% Wagner, Dell, Eisenman, JPO (2017), henceforth WDE17
%
% Other articles making use of this model:
% Gone with the wind: How model biases skew iceberg meltwater distributions
% Wagner & Eisenman, GRL (2017).
%
% "On the representation of capsizing in iceberg models"
% Wagner, Stern, Dell, Eisenman, Ocean Model (2017)
%
% "Wave inhibition by sea ice enables trans-Atlantic ice rafting of debris
% during Heinrich Events"
% Wagner, Dell, Eisenman, Keeling, Severinghaus, EPSL, (submitted)
%
% The model computes the drift and decay of icebergs, by solving their
% motion analytically for given velocity and SST conditions. Those
% conditions are loaded first (see "input fields").
%
% Then model parameters and analytical expressions are set.
%
% Then run parameters (time, domain, space domain etc) are specified.
%
% Then the release locations are loaded (this is a .mat-file, constructed
% in "construct_seeding.m".
%
% Finally the different bergsizes are loaded and specified.
%
% Then we compute the Lagrangian trajectories, looping over every iceberg
% size class. These loops consist of 2 components for every timestep.
%
% 1) drifting - computes iceberg velocity and translation
% 2) melting  - computes change in iceberg dimensions and volume
%
% Trajectories for every iceberg size class are saved as individual files
% in the output trajectory.
%
% Till Wagner & Ian Eisenman, May 2017,
% tjwagner@ucsd.edu, eisenman@ucsd.edu

% -------------------------------------------------------------------------
% pick type of input ------------------------------------------------------
model = 'ECCO2';    %you can either run this with ECCO2 input fields or
% model = 'CCSM4';  %CCSM4 input fields
% -------------------------------------------------------------------------
% load input fields -------------------------------------------------------
% -------------------------------------------------------------------------
% ECCO2 data can be downloaded from
% ftp://ecco2.jpl.nasa.gov/data1/cube/cube92/lat_lon/quart_90S_90N/
% with JRA-25 surface wind reanalysis data
% (https://rda.ucar.edu/datasets/ds625.0/)
% -------------------------------------------------------------------------
% CCSM4 data can be downloaded from
% -------------------------------------------------------------------------
% the demo data set here is for the year 1992, and for the Atlantic only
% here we load surface ocean currents, winds, SST, and landmask from 1 file
load(strcat(model,'_1992_Atlantic.mat'))
fprintf('model data loaded \n')
% -------------------------------------------------------------------------
% Iceberg Parameters and analytical expressions of
% gamma, Lambda, alpha, beta (eqns 7,8,9)
% -------------------------------------------------------------------------
R = 6378*1e3;       % earth radius in m
rhow = 1027;        % density of water (kg/m^3)
rhoa = 1.2;         % density of air   (kg/m^3)
rhoi = 850;         % density of shelf ice (kg/m^3), Silva et al (2006)
drho = rhow-rhoi;
Cw = 0.9;           % bulk coefficient water (Bigg et al 1997)
Ca = 1.3;           % bulk coefficient air   (Bigg et al 1997)
Om = 7.2921*1e-5;   % rotation rate of earth (rad/s)
ff = @(lat) 2*Om*sin(abs(lat)*pi/180);  % latitude in degrees
ga  = sqrt(rhoa*drho/rhow/rhoi*Ca/Cw);  % gamma = sqrt(ca/cw), Eq. 7
S  = @(l,w) l.*w./(l+w);                % harmonic mean length
La = @(u,lat,S) Cw*ga/ff(lat).*u/S/pi;  % Lambda, Eq. 9
% alpha and beta (eq 8 in WDE17)-------------------------------------------
a = @(La) 1./(2*La.^3).*(sqrt(1+4*La.^4)-1);
b = @(La) 1./(sqrt(2)*La.^3).*sqrt((1+La.^4).*sqrt(1+4*La.^4)-3*La.^4-1);
% Melt parameters (WDE17 Appendix)-----------------------------------------
Ti = -4;
a1 = 8.7e-6; a2 = 5.8e-7;
b1 = 8.8e-8; b2 = 1.5e-8;
c  = 6.7e-6;
% -------------------------------------------------------------------------
% specify the space domain ------------------------------------------------
LAT = double(input.latw); LON = double(input.lonw);
if strcmp(model,'CCSM4')
    LAT2 = LAT(:); LON2 = LON(:);
    vec = reshape(1:length(LON2),151,192);
end
minLAT = min(LAT(:)); maxLAT = max(LAT(:));
minLON = min(LON(:)); maxLON = max(LON(:));
msk = input.landmask;
% -------------------------------------------------------------------------
% set run parameters ------------------------------------------------------
if strcmp(model,'ECCO2') % Input fields time step in days
    DT = 3;
else
    DT = 1;
end
trajnum = 25;            % total number of iceberg trajectories to compute
final_t = 366/DT;        % time span of input field
startrange = round(linspace(1,final_t/2,trajnum)); %evenly space seeding
dt = 24*3600;            % model timestep in seconds (dt = 1 day)
dtR = dt/R*180/pi;       % need this ratio for distances
% -------------------------------------------------------------------------
t = 1:final_t;                  %input time
nt= length(t)*DT;               %number of model timesteps
tt = linspace(1,length(t),nt);  %model time
% -------------------------------------------------------------------------
% Load Seeding fields -----------------------------------------------------
load(strcat(model,'_Laurentide_Seed'))
seed_X = repmat(Seed_X(:),[100,1]); %cycle through each location 100x
seed_Y = repmat(Seed_Y(:),[100,1]); %i.e. this can run 3600 icebergs
% -------------------------------------------------------------------------
% these are the circulation fields-----------------------------------------
uwF = input.uw(:,:,t); vwF = input.vw(:,:,t);   %water vels input
uaF = input.ua(:,:,t); vaF = input.va(:,:,t);   %air vels input
sst = input.sst(:,:,t);                         %sst vels input
% -------------------------------------------------------------------------
% loop over individual initial iceberg size classes -----------------------
% -------------------------------------------------------------------------
bvec = 1:10;   %vector of which size classes to compute - has to be [1,10]
bergdims=[100          67          67  % [L W H]
    200         133         133
    300         200         200
    400         267         267
    500         333         300
    600         400         300
    750         500         300
    900         600         300
    1200         800         300
    1500        1000         300];
% -------------------------------------------------------------------------
for bb = bvec
    % ---------------------------------------------------------------------
    bergsize = bb;   % current berg size class
    fprintf('run bergsize B%d \n',bergsize)
    % ---------------------------------------------------------------------
    XIL = nan(trajnum,nt); YIL = XIL; VOL = XIL;        %set output  arrays
    % ---------------------------------------------------------------------
    L = bergdims(bergsize,1); %initialize iceberg dimensions
    W = bergdims(bergsize,2);
    H = bergdims(bergsize,3);
    % ---------------------------------------------------------------------
    % run drift and melt---------------------------------------------------
    mm=0; ss=0; ob=0; %set counters for melted, survived, and escaped bergs
    for j = 1:trajnum
        if mod(j,10)==0; fprintf('%d trajectories computed \n',j); end
        
        ts = startrange(j);            %pick seeding time (of input field)
        tts= ts*DT;                    %trajectory start time (of model)
        lt = nt-tts;                   %trajectory run length
        
        xil = nan(1,lt); yil = xil;  v = xil;   %initialize output vectors
        yig = seed_Y(j); xig = seed_X(j);   % cycle through start locations
        if strcmp(model,'ECCO2')
            xil(1) = LON(xig); yil(1) = LAT(yig);   %initial lon and lat
        else
            xil(1) = LON(yig,xig); yil(1) = LAT(yig,xig);
        end
        l = L*ones(1,lt); w = l*W/L; h = l*H/L; %initial berg dimensions
        v(1) = L*W*H;                           %initial volume and dvol

        % integrate as long as the iceberg is in the domain and not
        % melted and over the time period specified above
        i = 0; outofbound = 0; melted = 0;
        while outofbound == 0 && melted == 0 && i<lt-1
            i = i+1;
            % -------------------------------------------------------------
            % Computes iceberg drift component, WDE17, Section 3
            % -------------------------------------------------------------
            % find nearest neighbor of surface condition field
            if strcmp(model,'ECCO2') % this only works on a regular grid
                YI = dsearchn(LAT,yil(i));
                XI = dsearchn(LON,xil(i));
            else  %CCSM4 is on an irregular grid
                indic = dsearchn([LAT2,LON2],[yil(i),xil(i)]);
                [XI,YI]=find(indic==vec);
            end
            % now interpolate fields linearly between timesteps------------
            timestep = tt(tts+i);
            t1  = floor(timestep); t2 = t1+1;
            dt1 = timestep-t1; dt2 = t2-timestep;
            ua = uaF(XI,YI,t1)*dt1+uaF(XI,YI,t2)*dt2;
            va = vaF(XI,YI,t1)*dt1+vaF(XI,YI,t2)*dt2;
            uw = uwF(XI,YI,t1)*dt1+uwF(XI,YI,t2)*dt2;
            vw = vwF(XI,YI,t1)*dt1+vwF(XI,YI,t2)*dt2;
            SST= sst(XI,YI,t1)*dt1+sst(XI,YI,t2)*dt2;
            % compute wind speed and Lambda at location (for given iceberg 
            % size)--------------------------------------------------------
            Ua = sqrt(ua^2+va^2);
            LA = La(Ua,yil(i),S(l(i),w(i)));
            % now compute iceberg velocity Eq (6)--------------------------
            ui = uw + ga*( a(LA)*va + b(LA)*ua);
            vi = vw + ga*(-a(LA)*ua + b(LA)*va);
            % iceberg translation (convert from m to deg lat/lon)----------
            dlon = ui*dtR;
            dlat = vi*dtR;
            yil(i+1) = yil(i) + dlat;
            xil(i+1) = xil(i) + dlon/cos((yil(i+1)+yil(i))/2*pi/180);
            % check you haven't gone out of bounds-------------------------
            if xil(i+1)>maxLON || xil(i+1)<minLON || ...
                    yil(i+1)>maxLAT || yil(i+1)<minLAT
                outofbound = 1;
                ob = ob+1;
                fprintf('iceberg %d left domain at timestep %d \n',j,i);
            else % now check iceberg not on land---------------------------
                if strcmp(model,'ECCO2')
                    yi2(1) = find(LAT<=yil(i+1),1,'last');
                    yi2(2) = find(LAT>yil(i+1),1,'first');
                    xi2(1) = find(LON<=xil(i+1),1,'last');
                    xi2(2) = find(LON>xil(i+1),1,'first');
                    % when new position within one grid box of land:
                    if any(find(msk(xi2,yi2)==0))
                        yil(i+1) = yil(i);  %iceberg reset to time step i
                        xil(i+1) = xil(i);
                    end
                else % similar for CCSM4
                    indic = dsearchn([LAT2,LON2],[yil(i+1),xil(i+1)]);
                    [XI,YI]=find(indic==vec);
                    if input.landmask(XI,YI)==1
                        yil(i+1) = yil(i);
                        xil(i+1) = xil(i);
                    end
                end
            end
            % -------------------------------------------------------------
            % Here we compute the iceberg melting component, WDE15 Appendix
            % -------------------------------------------------------------
            % Compute Melt Terms Eq A1 in WDE17----------------------------
            Me = a1*Ua^0.5 + a2*Ua;
            Mv = b1*SST + b2*SST^2;
            Mb = c*sqrt((ui - uw).^2+(vi-vw).^2)^0.8*(SST-Ti)*l(i)^(-0.2);
            % Apply Melt Rates --------------------------------------------
            dldt = - Mv - Me; dhdt = - Mb;
            l(i+1) = l(i)+dldt*dt;
            w(i+1) = w(i)+dldt*dt;
            h(i+1) = h(i)+dhdt*dt;
            % -------------------------------------------------------------
            % Make sure the berg is not negative size
            if l(i+1)<0 || w(i+1)<0 || h(i+1)<0
                l(i+1)=0; w(i+1)=0; h(i+1)=0;
                melted = 1;
                mm = mm+1;
            end
            % Rollovers (Eq A2 in WDE17)
            if w(i+1)/h(i+1) < sqrt(6*rhoi/rhow*(1-rhoi/rhow))
                hn = w(i+1); w(i+1) = h(i+1); h(i+1) = hn;
            end
            % Make sure length>width:
            if w(i+1)>l(i+1)
                wn = l(i+1); l(i+1)=w(i+1); w(i+1) = wn;
            end
            % Compute new volume
            v(i+1) = l(i+1)*w(i+1)*h(i+1);
            % Check whether iceberg survived-------------------------------
            if i == lt-1 && v(i+1) > 0
                ss = ss+1;
            end
        end
        ind = 1:i+1;
        XIL(j,ind)=xil(ind); YIL(j,ind)=yil(ind);   %store trajectory
        VOL(j,ind)=v(ind);                          %store volume
    end
    % ---------------------------------------------------------------------
    fprintf('%d icebergs died, %d lived, %d left the domain \n',mm,ss,ob)
    % ---------------------------------------------------------------------
    save(sprintf('%s_Size%d',model,bb),'XIL','YIL','VOL'); %save output
end
% -------------------------------------------------------------------------
% Plot Iceberg Trajectories------------------------------------------------
%
% This loads in output files from "iceberg_model.m" for different iceberg
% sizes and plots them (colorcoded). 
%
% Requires the matlab mapping toolbox. I made the atl.mat file to quickly
% plot a low-resolution landmask of the north atlantic. 
% -------------------------------------------------------------------------

figure(1); clf
% make North Atlantic Map plot (requires mapping toolbox)------------------
load atl
latlim = [33 70];
lonlim = [-80 0];
worldmap(latlim,lonlim)
gri = gridm('off');
geo = geoshow(atl(1:10), 'FaceColor', [.9 .9 .9]);
setm(gca,'FFaceColor',[1 1 1])
hold on
hm = mlabel('on');
set(hm(:),'fontsize',13);
set(hm(:),'verticalalignment','baseline');
pm = plabel('on');
set(pm(:),'fontsize',13);
set(pm(:),'verticalalignment','baseline');

%uncomment the following if you've precomputed output files and -----------
%want to call just the plotting cell --------------------------------------
% model = 'ECCO2'; 

N = 25;   %number of trajectories to be plotted
bvec = [9 7 5 3 1];  %specify different iceberg sizes you're interested in
p = cell(1,length(bvec)); pl = p; leg = p;
ii = 0;
for i  = bvec
    load(sprintf('%s_Size%d',model,i))
    bb = i; bs = bb;
    XL = XIL;
    YL = YIL;
    ind = 1:N;
    ii = ii+1;
    for tind = 1:length(ind)
        vend = find(VOL(ind(tind),:)<.1*VOL(ind(tind),1),1,'first'); 
        %plot until iceberg is 90% decayed
        if isempty(vend)
            vend = size(VOL,2);
        end
        if tind == 1
            p{ii} = plotm(YL(ind(1),1:vend)',XL(ind(1),1:vend)'...
                ,'-','col',1-[bb/15 1-bb/15 1-bb/15]);
        else
            plotm(YL(ind(tind),1:vend)',XL(ind(tind),1:vend)'...
                ,'-','col',1-[bb/15 1-bb/15 1-bb/15]);
        end
    end
end

for i = 1:ii; pp = p{i}; pl{i}=pp(1); leg{i} = sprintf('%d',bvec(i)); end
ll = legend([pl{:}],leg(:),'location','northeastoutside',...
    'interpreter','latex');
legendtitle(ll,'Size Class','fontweight','normal');
title(sprintf('%s Forcing',model))