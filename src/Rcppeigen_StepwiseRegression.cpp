// we only include RcppEigen.h which pulls Rcpp.h in for us
#include <RcppEigen.h>
#include <math.h>	   // sqrt
#include <algorithm>	// std::min
using namespace std;
using std::string;
using namespace Eigen;

//AIC-AICc_BIC-SBC-HQ-HQc
//*---get Residuals Sum of Squarts via Matrix Operation
// [[Rcpp::export]]
double InforCriteria(double SSE,double nY, int p, int n, string criteria, double sigma,double SST){ //--nY is double variable and return double results
	double constTwo=2.0, InfCrit=0.0; 
	if(criteria=="SBC"){
		InfCrit = n*log(SSE/n)+log(n)*p;
	}
	if(criteria=="AIC"){
		InfCrit = n*log(SSE/n)+(2*p*nY*n+nY*(nY+1))/n-constTwo/n+n+2;
	}
	if(criteria=="AICc"){
		InfCrit = n*log(SSE/n)+n*(n+p)*nY/(n-p-nY-1);
	}
	if(criteria=="HQ"){
		InfCrit = n*log(SSE/n)+2*log(log(n))*p*nY/n;
	}
	if(criteria=="HQc"){
		InfCrit = n*log(SSE*SSE/n)+2*log(log(n))*p*nY/(n-p-nY-1);
	}
	if(criteria=="Rsq"){
		InfCrit = 1-SSE/SST;
	}
	if(criteria=="adjRsq"){
		InfCrit = 1-(SSE/SST)*(n-1)/(n-p);
	}
	// BIC and CP should call Sigma_RCpp function
	if(criteria=="BIC"){
		InfCrit = n*log(SSE/n)+2*(2+p)*(n*sigma/SSE)-2*(n*sigma/SSE)*(n*sigma/SSE);
	}
	if(criteria=="CP"){
		InfCrit = SSE/sigma+2*p-n;
	}
	return InfCrit;
}
//get P value from F value and df1 df2---------------------------start
double c[11] = { 0.0000677106, -0.0003442342, 0.0015397681, -0.0024467480,
		0.0109736958, -0.0002109075, 0.0742379071, 0.0815782188, 0.4118402518,
		0.4227843370, 1.0000000000 };
double gammaln(double xx) {
	double x, y, tmp, ser;
	static const double cof[6] = {
		76.18009172947146,
		-86.50532032941677,
		24.0140982408091,
		-1.231739572460155,
		0.1208650973866179e-2,
		-0.5395239384953e-5
	};
	y = x = xx;
	tmp = (x + 0.5) * log(x + 5.5) - (x + 5.5);
	ser = 1.000000000190015;
	for (int j = 0; j < 6; j ++) {
		ser += cof[j] / (y + 1);
		y = y + 1;
	}
	return tmp + log(2.5066282746310005 * ser / x);
}
double beta(double x, double y) {
	if (x <= 0 || y <= 0) {
		return 0;
	}
	return exp(gammaln(x)+gammaln(y)-gammaln(x + y));
}
double fi(int N, double x, double a, double b) {
	int n = N / 2;
	double f = 0.0, f1, s1, s2, tmpU, tmpV;
	int i;
	for (i = n; i >= 1; i--) {
		tmpU = (a + 2.0 * i - 1.0) * (a + 2.0 * i);
		s2 = i * (b - i) * x / tmpU;
		f1 = s2 / (1.0 + f);
		tmpV = (a + 2.0 * i - 2.0) * (a + 2.0 * i - 1.0);
		s1 = -(a + i - 1.0) * (b + a + i - 1.0) * x / tmpV;
		f = s1 / (1.0 + f1);
	}
	return 1.0 / (1.0 + f);
}
double incomBeta(double x, double a, double b) {
	if (a <= 0.0 || b <= 0.0) {
		return 0.0;
	}
	if (abs(x - 0.0) < 1.0e-30 || abs(x - 1.0) < 1.0e-30) {
		return 0.0;
	}
	double c1, c2, c3, f1, f2;
	int n;
	//c1 = pow(x, a);
	//c2 = pow(1.0 - x, b);
	//c3 = beta(a, b);
	c1=a*log10(x);
	c2=b*log10(1-x);
	c3=log10(beta(a, b));
	if (x < (a + 1.0) / (a + b + 2.0)) {
		n = 1;
		while (1) {
			f1 = fi(2 * n, x, a, b);
			f2 = fi(2 * n + 2, x, a, b);
			if (abs(f2 - f1) < 1.0e-30)
				return (log10(f2) + c1 + c2 - log10(a) - c3);
			else
				n++;
		}
	} else {
		if (abs(x - 0.5) < 1.0e-30 && abs(a - b) < 1.0e-30)
			return 0.5;
		else {
			n = 1;
			while (1) {
				f1 = fi(2 * n, 1.0 - x, b, a);
				f2 = fi(2 * n + 2, 1.0 - x, b, a);
				if (abs(f2 - f1) < 1.0e-30)
					return  - (log10(f2) + c1 + c2 - log10(b) - c3);
				else
					n++;
			}
		}
	}
	return 0;
}
double Pval_RCpp(double f, double n1, double n2) {
	if (f < 0.0)
		f = -f;
	return incomBeta(n2 / (n2 + n1 * f), n2 / 2.0, n1 / 2.0);
}
//-------------remove any column in a matrix---------------------
Eigen::MatrixXd removeColumn(const Eigen::MatrixXd& matrix, unsigned int colToRemove,int p){
	unsigned int numRows = matrix.rows();
	unsigned int numCols = matrix.cols()-p;

	Eigen::MatrixXd X;
	X=matrix;
	if( colToRemove < numCols )
		X.block(0,colToRemove,numRows,numCols-colToRemove) = matrix.block(0,colToRemove+p,numRows,numCols-colToRemove);

	X.conservativeResize(numRows,numCols);
	return X;
}
// [[Rcpp::export]]
Rcpp::List stepOne(bool findIn,int p,int n,double sigma,double tolerance, string Ftrace, string criteria, const Eigen::MatrixXd& Y,const Eigen::MatrixXd& X1,const Eigen::MatrixXd& X0,int k,double SST){
	double initPIC;  //minimum or maximum of P-value or Information criteria value
	if("SL"==criteria || "Rsq"==criteria || "adjRsq"==criteria){
		if(false==findIn){
			initPIC = 0;
		}else{
			initPIC = -1e10;   
		}
	}else{
		initPIC = 1e+10;  
	}
	int pointer=-2,nR,nD,rank0,rank1=0,rank2,rank_r=0,rank_f=0,maxCols,ncol; 
	double PIC,RSSrd=0.0,RSSfd=0.0,RSSXd,SSE,F_value,nY=Y.cols(),p1,q1,s1,v1,m1,n1,TraceV,t1,v11,v22;
	Eigen::FullPivLU< Eigen::MatrixXd > luX0(X0);
	luX0.setThreshold(tolerance); 
	rank0=luX0.rank();
	Eigen::MatrixXd image0 = luX0.image(X0);
	const Eigen::MatrixXd B0 = image0.colPivHouseholderQr().solve( Y ); 
	const Eigen::MatrixXd Y_XB0 = Y - image0 * B0;
	nR=n-rank0;
	Eigen::MatrixXd RSSr;
	Eigen::MatrixXd RSSf;
	if(false==findIn){
		//Xr = X0
		rank_r = rank0;
		RSSr = Y_XB0.transpose() * Y_XB0;
		RSSrd=RSSr.determinant();
		maxCols = X1.cols()/p;
		ncol = p;
	}else{
		//Xf = X0
		rank_f = rank0;
		RSSf = Y_XB0.transpose() * Y_XB0;
		RSSfd=RSSf.determinant();
		maxCols = (X0.cols()-1)/p-k;
		ncol = -p;
	}
	Eigen::MatrixXd X(n,X0.cols() + ncol);

	for(int i = 0; i < maxCols; i++){
		if(false==findIn){
			//Xf = X
			X << X0,X1.middleCols(i*p,p);  //X1.middleCols(0,0)=b   
		}else{
			//Xr = X
			 X =  removeColumn(X0,p*(i+k)+1,p); 
		}
		Eigen::FullPivLU< Eigen::MatrixXd > luX(X);
		luX.setThreshold(tolerance); 
		rank1=luX.rank();
		Eigen::MatrixXd image1=luX.image(X);
		const Eigen::MatrixXd B = image1.colPivHouseholderQr().solve( Y ); 
		const Eigen::MatrixXd Y_XB = Y - image1 * B;
 		Eigen::MatrixXd RSSX = Y_XB.transpose() * Y_XB;
		RSSXd = RSSX.determinant(); 

		if(false==findIn){
			rank_f = rank1;
			RSSf = Y_XB.transpose() * Y_XB;
			RSSfd=RSSf.determinant(); 
		}else{
			rank_r = rank1;
			RSSr = Y_XB.transpose() * Y_XB;
			RSSrd=RSSr.determinant(); 
		}
		nD = rank_f - rank_r;
		if(nD != 0){
			if("SL"==criteria){   
				if(nY >1){
					Eigen::MatrixXd H = RSSr-RSSf;
					Eigen::MatrixXd E = RSSf;
					p1=nY;
					q1=nD;
					s1=min(p1,q1);
					v1=n-rank_f;
					m1=(abs(p1-q1)-1)*0.5;
					n1=(v1-p1-1)*0.5;
					if("Pillai"==Ftrace){
						TraceV=(H*RSSr.inverse()).trace();
						v11=s1*(2*m1+s1+1);
						v22=s1*(2*n1+s1+1);
						F_value=(2*n1+s1+1)*TraceV/((2*m1+s1+1)*(s1-TraceV));
						PIC = Pval_RCpp(F_value,v11,v22);
						if(PIC<initPIC){
							initPIC=PIC;
							pointer=i;
							SSE=RSSXd;
							rank2=rank1;
						}
					}
					if("Hotelling"==Ftrace){
						TraceV=(H*E.inverse()).trace();
						v11=s1*(2*m1+s1+1);
						v22=2*(s1*n1+1);
						F_value=2*(s1*n1+1)*TraceV/(s1*s1*(2*m1+s1+1));
						PIC = Pval_RCpp(F_value,v11,v22);
						if(PIC<initPIC){
							initPIC=PIC;
							pointer=i;
							SSE=RSSXd;
							rank2=rank1;
						}
					}
					if("Wilks"==Ftrace){
						TraceV=RSSfd/RSSrd;
						v11=p1*q1;
						double u1 = (v11-2)*0.25;
						double r1 = v1-(p1-q1+1)*0.5;
						double k1 = p1*p1+q1*q1-5;
					  	if(k1>0){
							t1=sqrt((v11*v11-4)/k1); //how to express sqrt()
						}else{
							t1=1;
						}
						v22=r1*t1-2*u1;
						double stat=pow(TraceV,1/t1);
						F_value=(1-stat)*v22/(stat*v11);	//error in :F_value=pow(1-Wilks,1/t1)*(r1*t1-2*u1)/(pow(Wilks,1/t1)*p1*q1);
						PIC = Pval_RCpp(F_value,v11,v22); 
						if(PIC<initPIC){
							initPIC=PIC;
							pointer=i;
							SSE=RSSXd;
							rank2=rank1;
						}
					}
				}else{
					F_value = (RSSrd-RSSfd)/(rank1-rank0)/(RSSfd/(n-rank1-1));
					PIC = Pval_RCpp(F_value,nD,nR); 
					if(PIC<initPIC){
						initPIC=PIC;
						pointer=i;
						SSE=RSSXd;
						rank2=rank1;
					} 
				}
			}else if("Rsq"==criteria || "adjRsq"==criteria){
				PIC = InforCriteria(RSSXd,nY,rank1,n,criteria,sigma,SST); 
				if(PIC>initPIC){
					initPIC=PIC;
					pointer=i; 
					SSE=RSSXd;
					rank2=rank1;
				}
			}else{
				PIC = InforCriteria(RSSXd,nY,rank1,n,criteria,sigma,SST); 
				if(PIC<initPIC){
					initPIC=PIC;
					pointer=i; 
					SSE=RSSXd;
					rank2=rank1;
				}
			}//criteria 
		}
	}
  return Rcpp::List::create(Rcpp::Named("PIC")=initPIC,
							Rcpp::Named("SEQ")=pointer+1,
							Rcpp::Named("SSE")=SSE,
							Rcpp::Named("rank0")=rank0,
							Rcpp::Named("rank")=rank2);
}
