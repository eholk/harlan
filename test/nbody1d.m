function [Fx, Fy, Fz, Vx, Vy, Vz, gpu_Fx, gpu_Fy, gpu_Fz, gpu_Vx, gpu_Vy, gpu_Vz] = nbody1d(n, Rx, Ry, Rz, m, dT, T)
%ParaMType n = 1000
%ParaMType  IXF_n = 'i'
%ParaMType  SXF_n = [1 1]
%ParaMType  IXF_Rx = 'r'
%ParaMType  SXF_Rx = [n 1]
%ParaMType  IXF_Ry = 'r'
%ParaMType  SXF_Ry = [n 1]
%ParaMType  IXF_Rz = 'r'
%ParaMType  SXF_Rz = [n 1]
%ParaMType  IXF_m = 'r'
%ParaMType  SXF_m = [n 1]
%ParaMType  dT  = 0.5 
%ParaMType  IXF_dT  = 'r'
%ParaMType  SXF_dT  = [1 1]
%ParaMType  T = 200 
%ParaMType  IXF_T = 'r'
%ParaMType  SXF_T = [1 1]
%-----------------------------------------------------------------------
%
%	This function M-file simulates the gravitational movement
%	of a set of objects.
%
%	Invocation:
%		>> [Fx, Fy, Fz, Vx, Vy, Vz] = ...
%		   nbody1d(n, Rx, Ry, Rz, m, dT, T)
%
%		where
%
%		i. n is the number of objects,
%
%		i. Rx is the n x 1 radius vector of x-components,
%
%		i. Ry is the n x 1 radius vector of y-components,
%
%		i. Rz is the n x 1 radius vector of z-components,
%
%		i. m is the n x 1 vector of object masses,
%
%		i. dT is the time increment of evolution,
%
%		i. T is the total time for evolution,
%
%		o. Fx is the n x 1 vector of net x-component forces,
%
%		o. Fy is the n x 1 vector of net y-component forces,
%
%		o. Fz is the n x 1 vector of net z-component forces,
%
%		o. Vx is the n x 1 vector of x-component velocities,
%
%		o. Vy is the n x 1 vector of y-component velocities,
%
%		o. Vz is the n x 1 vector of z-component velocities.
%
%	Requirements:
%		None.
%
%	Examples:
%		>> [Fx, Fy, Fz, Vx, Vy, Vz] = ...
%		   nbody1d(n, ...
%		   rand(n, 1)*1000.23, ...
%		   rand(n, 1)*1000.23, ...
%		   rand(n, 1)*1000.23, ...
%		   rand(n, 1)*345, 0.01, 20)
%
%	Source:
%		Quinn's "Otter" project.
%
%	Author:
%		Alexey Malishevsky (malishal@cs.orst.edu).
%
%-----------------------------------------------------------------------

Fx = zeros(n, 1);
Fy = zeros(n, 1);
Fz = zeros(n, 1);

Vx = zeros(n, 1);
Vy = zeros(n, 1);
Vz = zeros(n, 1);

G = 1e-11; % Gravitational constant.

for t1 = 1:dT:T,
    %ParaMType  IXF_t = 'r'
    %ParaMType  SXF_t = [1 1]
    t = t1;
    for t2 = 1:n,
    %ParaMType  IXF_k = 'i'
    %ParaMType  SXF_k = [1 1]
      k=t2;
	% Find the displacement vector between all particles
	% and the kth particle.
	drx = Rx-Rx(k);
	dry = Ry-Ry(k);
	drz = Rz-Rz(k);
	% Find the squared distance between all particles
	% and the kth particle, adjusting "self distances" to 1.
	r = drx.*drx+dry.*dry+drz.*drz;
	r(k) = 1.0;
	% Find the product of the kth particle's mass and
	% and every object's mass, adjusting "self products" to 0.
	M = m*m(k);
	M(k) = 0.0;
	% Find the gravitational force.
	f = G*(M./r);
	% Find the unit direction vector.
	r = sqrt(r);
	drx = drx./r;
	dry = dry./r;
	drz = drz./r;
  frx = f.*drx;
  fry = f.*dry;
  frz = f.*drz;
	% Find the resulting force.
%ParaMType  IXF_tmp1  = 'r'
%ParaMType  SXF_tmp1  = [1  1]
  tmp1  = sum(frx);
%ParaMType  IXF_tmp2  = 'r'
%ParaMType  SXF_tmp2  = [1  1]
  tmp2  = sum(fry);
%ParaMType  IXF_tmp3  = 'r'
%ParaMType  SXF_tmp3  = [1  1]
  tmp3  = sum(frz);
  Fx(k) = tmp1/length(frx);
  Fy(k) = tmp2/length(frx);
  Fz(k) = tmp3/length(frx);
	%Fx(k) = sum(frx)/length(frx);
  %Fx(k) = Fx(k)*n;
	%Fy(k) = sum(fry)/length(fry);
  %Fy(k) = Fy(k)*n;
	%Fz(k) = sum(frz)/length(frz);
  %Fz(k) = Fz(k)*n;
   end;
    % Find the acceleration.
    ax = Fx./m;
    ay = Fy./m;
    az = Fz./m;
    % Find the velocity.
    Vx = Vx;
    Vy = Vy;
    Vz = Vz;
    Vx = Vx+ax*dT;
    Vy = Vy+ay*dT;
    Vz = Vz+az*dT;
    % Find the radius vector.
    Rx = Rx;
    Ry = Ry;
    Rz = Rz;
    Rx = Rx+Vx*dT;
    Ry = Ry+Vy*dT;
    Rz = Rz+Vz*dT;
end;
