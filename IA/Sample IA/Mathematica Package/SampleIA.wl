(* ::Package:: *)

BeginPackage["SampleIA`"]
Needs["NumericalCalculus`"]
Unprotect @@ Names["SampleIA`*"];
ClearALL @@ Names["SampleIA`*"];



graphGenerator::usage = "Makes a kewl graph "
uncertaintyDataGenerator::usage = "Does something" 
linearSlopeWUncertainty::usage = "Words "
linCoefCalc::usage = "calculates slop of 2 points"
rsquared::usage = "calculates r squared coefficient "
simplePointsGenerator::usage = "makes new POINTS"
x::usage = "func"


Begin["Private`"]



x[z_]:=z+1;

(* Sets the "optional" input defaults. Proportional fit is false, default graph x and y axis labels*)

Options[graphGenerator] = {proportional -> False, xl -> "t (h)", yl -> "l (cm)", pointsChoice->{1,-3}};
SetOptions[graphGenerator, proportional -> False, xl -> "t (h)", yl -> "l (cm)",pointsChoice->{1,-3}];


(* This is the main function that puts everything together (all of the graphs). It requires only one input, a list of points with their uncertainty*) 
graphGenerator[pointsList_List, OptionsPattern[]] :=

(*I use the following formatting for my dataset of points with uncertainties.
{ {  {x1,delta x1 }, {y1,delta y1} }, {{x2,delta x2},{y2,delta y2}}  ..... } *) 


(* Block, is used to enclose a piece of code where you introduce new variables that you only want to use in that part of code. 
They are listed in a separate array in the beginning of the block, and separated from the rest of the code by a comma*)
    Block[{pointsForGraph, rawgraph, fits, kexac, bexac, set1, set2, exacfit, x, fit1, fit2, xmin, xmax, check, i, graph, xlabel, ylabel,choiceOfPoints},
    
    
    (* Gets all of the data for the optinal input. If no option is specified, uses default values*)
        check = OptionValue[proportional]; 
        xlabel = OptionValue[xl];
        ylabel = OptionValue[yl];
        
        choiceOfPoints = OptionValue[pointsChoice];
  
         
        (* We are unable to directly plot our points from the format they are currently in. We also want to turn all numbers into the Around type, so we can
        plot a graph with error bars. uncertaintyDataGenerator is my own made function. *)
        pointsForGraph = uncertaintyDataGenerator[pointsList]; 
        
        
        (* We plot this data with error bars *)
        rawgraph = ListPlot[pointsForGraph]; 
        
      
        
          (* Now we want to return to our original style data and calculate the min/max and exact slopes. We specify what kind of linear fit we want
          (proportional or not).We also add the option of choosing which points are used to calculated
        min max uncertainty  *) 
          
          (*This outputs an array of the following format *) 
          (* { {slope, slope uncertainty }, {y intercept, y intercept uncertainty} }  ,{{minimum slope, max y intercept },{ maximum slope, minimum y intercept }, rsquared coefficient   }    } *) 
          
          fits = linearSlopeWUncertainty[pointsList, proportional -> check, pointsChoice->choiceOfPoints];
          
         
          
          (*Now we are ready to plot lines of best fit. To do so, we need to find out the domain over which we will plot these lines.
          I create two variables for  the minimum value of x and maximum value of x. I set them both to be the first x value in our data  *) 
          
           xmin = pointsList[[1]][[1]][[1]];
            xmax = pointsList[[1]][[1]][[1]];
           
           (* We then go through all of the data points. If an x coordinate is greater or smaller than the currently set min/max, it gets updated. *)
            For[i = 1, i <= Length[pointsList], i++, 
            If[xmin > pointsList[[i]][[1]][[1]],
                xmin = pointsList[[i]][[1]][[1]]
            ];
            
              If[xmax < pointsList[[i]][[1]][[1]],
                xmax = pointsList[[i]][[1]][[1]]
            ];
        ]; 
        
        (* If we are calulating a proportional fit, then we will assume 0 to be the beginning of our domain (unless there are negative xs). *) 
        If[check == True && xmin>0,
            xmin = 0
        ]; 
        (* I rename the coefficients so that they are easier to reference in the future *) 
        
        (* {slope, slope uncertainty} *)
        kexac = fits[[1]][[1]];
        
        (* {y intercept, y intercept uncertainty } *) 
        bexac = fits[[1]][[2]];
        
         (*  {minimum slope, maximum y intercept}     *) 
         set1 = fits[[2]][[1]];
         
         (* {maximum slope, minimum y intercept} *)
          set2 = fits[[2]][[2]];
          
          (* Plot the a line of best fit with exact coefficients. Extend domain by 1/100 of the biggest x point*) 
           exacfit = Plot[kexac[[1]] * x + bexac[[1]], {x, xmin,xmax+ xmax / 100}]; 
           
           
           (* Plot min and max lines of best fit on same domain*)
           fit1 = Plot[set1[[1]] * x + set1[[2]], {x, xmin, xmax+ xmax / 100}, PlotStyle -> {Dashed}];
           fit2 = Plot[set2[[1]] * x + set2[[2]], {x, xmin, xmax + xmax / 100}, PlotStyle -> {DotDashed}]; 
         
          (* combine all graphs together and set the labels*) 
           graph = Show[rawgraph, exacfit, fit1, fit2, AxesOrigin -> {0, 0}, FrameLabel -> {{Style[ylabel, Black, FontFamily -> "Courier New", 12], None}, {Style[xlabel, Black, FontFamily -> "Courier New", 12], None}}, Frame -> {{True, False}, {True, False}}]; 
          (* return the graph, the linear coefficient swith their uncertainties and rsquared value*)
            Return[{graph, {kexac, bexac},fits[[2]][[3]]}]
    ];
    
    (* This takes our list of points with uncertainty and turns it into a list of 2d points with uncertainty in the Around form*)
    uncertaintyDataGenerator[pointslist_List] :=
    
    (* Remember that the list has the following structure
     { { {x,delta x},{y,delta y } } ,{ {x2,deltax2},{y2,deltay2} } }
    
    *)
    
    Block[{aroundList, i,temp},
        aroundList = {}; (*Initialize new list*)
        
          For[i = 1, i <= Length[pointslist], i++, 
          (* Transfer all points in old list to new list in new format*) 
          temp ={ Around[pointslist[[i]][[1]][[1]], pointslist[[i]][[1]][[2]] ], Around[pointslist[[i]][[2]][[1]], pointslist[[i]][[2]][[2]]  ]    } ;
            aroundList = Append[aroundList, temp];
        ];
        
        (* Returns list *) 
         Return[aroundList]
    ]








Options[linearSlopeWUncertainty]={proportional->False, pointsChoice->{1, -3} };
SetOptions[linearSlopeWUncertainty,proportional->False,pointsChoice->{1, -3} ];
linearSlopeWUncertainty[pointslist_List, OptionsPattern[]] := Block[{z,normalPoints,p1,func,p2,k,rsq,b,fitpoints,deltak,deltab,x,mincoef,maxcoef,propcheck,equation,point0,pointl,minpointset,maxpointset,x0min,x0max,x1min,x1max,y0min,y0max,y1min,y1max,kn},
		
		
		x0min;
		x0max;
		x1min;
		x1max;
		y0min;
		y0max;
		y1min;
		y1max;
		
		
	propcheck = OptionValue[proportional];
	
	(* This option let's you choose which points are used to calculate min and max *)
	p1 = OptionValue[pointsChoice][[1]];
	
	p2 = OptionValue[pointsChoice][[2]];
	If[p2==-3, p2= Length[pointslist]];

	
	point0 = pointslist[[p1]];
	pointl = pointslist[[p2]];
	
	
		k;
		b;
		deltak;
		deltab;
	
	
	fitpoints = {};
	
	(* Makes a list of points without uncertainty. This is needed for fit function to work*)
	For[kn=1,kn<= Length[pointslist],kn++,
				
				fitpoints=Append[fitpoints,{ pointslist[[kn]][[1]][[1]],pointslist[[kn]][[2]][[1]]} ];
	];
	
	(*  Perfect Linear Fit   *)
		(* Calculates line of best fit *)
				If[propcheck == False,
	
				equation = Fit[fitpoints,{1,x},x];
				k = Coefficient[equation,x,1];
				b = Coefficient[equation,x,0];
				 ];
				
				(* Calculates proportional line of best fit*)
				If[propcheck == True,
				equation = Fit[fitpoints,{x},x];
				k = Coefficient[equation,x,1];
				b= Coefficient[equation,x,0];
				
				];
			(* Finds min max lines of best fit if line of best fit has positive slope*)
								(* max: rb-lt, min: lt-rb  *) 
							
							If[k>0,
									
									y0max = point0[[2]][[1]]-point0[[2]][[2]] ;
									x0max = point0[[1]][[1]]+point0[[1]][[2]];
									
									y1max= pointl[[2]][[1]]+pointl[[2]][[2]];
									x1max= pointl[[1]][[1]]-pointl[[1]][[2]];
									
									
									
									y0min= point0[[2]][[1]]+point0[[2]][[2]] ;
									x0min= point0[[1]][[1]] - point0[[1]][[2]];
									
									y1min= pointl[[2]][[1]]-pointl[[2]][[2]];
									x1min= pointl[[1]][[1]]+pointl[[1]][[2]];
									(*If a proportional fit is required, sets first point to be (0;0) *)
									If[propcheck==True,
									x0max = 0;
									x0min= 0;
									y0min= 0 ;
									y0max = 0 ;
									];
										
							];
				
								(* max: rt-lb, min: lb-rt  *)
								(* Finds min max lines of best fit if line of best fit has negative slope*)
							If[k<0,
									y0max = point0[[2]][[1]]+point0[[2]][[2]];
									x0max= point0[[1]][[1]]+point0[[1]][[2]];
									
									y1max= pointl[[2]][[1]]-pointl[[2]][[2]];
									x1max= pointl[[1]][[1]] - pointl[[1]][[2]];
									
									
									
									y0min= point0[[2]][[1]]-point0[[2]][[2]];
									x0min = point0[[1]][[1]]-point0[[1]][[2]];
									
									y1min= pointl[[2]][[1]] + pointl[[2]][[2]];
									x1min= pointl[[1]][[1]] +pointl[[1]][[2]]  ;
									
									If[propcheck==True,
									x0max = 0;
									x0min= 0;
									y0min= 0 ;
									y0max = 0 ;
									];
							
							];
				
					
				
				minpointset = { {x0min,y0min},{x1min,y1min} };
				maxpointset = {{x0max,y0max},{x1max,y1max}};
	
		(*Calculates line of best fit with minimum slope *) 
		mincoef = linCoefCalc[minpointset];
		(*Calculates line of best fit with maximum slope *)
		maxcoef= linCoefCalc[maxpointset];
		(*Write down line of best fit as function to calculate rsquared later *) 
	func[z_] := k*z+b;
	
			(* Find uncertainty of coefficients*)
			deltak = Max[Abs[k-mincoef[[1]]], Abs[k-maxcoef[[1]]]];
			deltab = Max[Abs[b-mincoef[[2]]], Abs[k-mincoef[[2]]]];
			
			normalPoints = simplePointsGenerator[pointslist]; 
			
			(*Calculates rsquared*)
			rsq=rsquared[normalPoints,func];
			
			Return[{{{k,deltak},{b,deltab}},{mincoef,maxcoef,rsq}}]
	]
	
	
	
    
    (* Makes a list of points without uncertainty*)
    simplePointsGenerator[pointslist_List] := Block[{ finaloutput,counts},
    
    finaloutput = {};
    
    For[counts=1,counts<= Length[pointslist], counts++,
    
     finaloutput =Append[finaloutput, {pointslist[[counts]][[1]][[1]], pointslist[[counts]][[2]][[1]]}];
    
    ];
    
    Return[finaloutput]
    ];
    
    



(* Finds slope and y intercept of line between two points *)
linCoefCalc[points_List] :=  Block[{x0,y0,x1,y1,slop,b},
	
		x0=points[[1]][[1]];
		y0=points[[1]][[2]];
		
		x1=points[[2]][[1]];
		y1=points[[2]][[2]];
		
		slop = (y1-y0)/(x1-x0);
		
		b= y1-slop*x1;
		Return[{slop,b}]
]


(* Calculates Rsquared coefficient for a mathematical model. Copied this off the internet*)

rsquared[list___,model_]:=1-SquaredEuclideanDistance[list[[;;,2]],model/@list[[;;,1]]]/SquaredEuclideanDistance[list[[;;,2]],Mean@list[[;;,2]]];



End[]
Protect @@ Names["SampleIA`*"];
EndPackage[] 

