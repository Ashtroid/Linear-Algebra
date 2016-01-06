import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
//Solves all nxn linear equations by transforming them into matrices and using LU decomposition
public class EquationSolver 
{
	int numRow, numCol;
	double[][]matrix, L, testMatrix;
	ArrayList<ArrayList<Double>>entries = new ArrayList<ArrayList<Double>>();		
	Map<Integer, String>variables = new HashMap<Integer, String>(10);
	ArrayList<Double>solutions = new ArrayList<Double>();
	ArrayList<Double>testSolutions = new ArrayList<Double>();
	ArrayList<Double>vars= new ArrayList<Double>();
	public EquationSolver()
	{
		numRow = 0;
		numCol = 0;
	}
	public void addLine(String line)
	{
		boolean isSolution = false;
		numRow++;
		String use = "";
		double temp = 1;
		ArrayList<Double>currentRow = new ArrayList<Double>();
		ArrayList<String>names = new ArrayList<String>();
		for(int i = 0; i < line.length(); i++)
		{	
			char comp = line.charAt(i);
			if(comp >='0' && comp <= '9')
			{
				use += comp - '0';
			}
			else if(comp == '-' || comp == '.')
			{
				use += comp;
			}
			else if(comp == '+')
			{
				temp = 1;
			}
			else if(comp == ' ' || comp == '*')
			{
				
			}
			else if(comp == '=')
			{
				temp = 1;
				isSolution = true;
			}
			else
			{
				if(!isSolution)
				{
					String varName = line.substring(i,i+1);
					if(!variables.containsValue(varName))
						variables.put(numCol++, varName);
					if(!use.isEmpty())
						temp = Double.parseDouble(use);
					currentRow.add(temp);
					names.add(varName);
					use = "";
				}
			}
		}
		if(isSolution)
		{
			if(!use.isEmpty())
				temp = Double.parseDouble(use);
			solutions.add(temp);
			use = "";
		}
		orderInts(currentRow,names);
	}
	//made by Ashwin Sethi
	private void orderInts(ArrayList<Double>currentRow, ArrayList<String>names)
	{
		ArrayList<Double>newList = new ArrayList<Double>();
		for(int i = 0; i < variables.size(); i++)
		{ 
			String findVar = variables.get(i);
			boolean isFound = false;
			for(int t = 0; t<names.size(); t++)
			{
				if(names.get(t).equals(findVar))
				{
					newList.add(currentRow.get(t));
					isFound = true;
				}
			}
			if(!isFound)
				newList.add((double) 0);
		}
		entries.add(newList);
	}
	public void createMatrix()
	{
		if(numRow == 0)
		{
			System.out.println("Empty\nProgram is now closing\n\n\n");
			return;	
		}
		matrix = new double[numRow][numCol];
		for(int a = 0; a < numRow; a++)
		{
			for(int b = 0; b < entries.get(a).size(); b++)
				matrix[a][b] = entries.get(a).get(b);
		}
		testMatrix = new double[numRow][numCol];
		for(int i = 0; i < numRow; i++)
			for(int j = 0; j < numCol; j++)
				testMatrix[i][j] = matrix[i][j];
		for(double x: solutions)
			testSolutions.add(x);
	}
	public void printMatrix()
	{
		System.out.println("Matrix:");
		for(int b = 0; b<variables.size(); b++)
		{
			System.out.print(variables.get(b) + "\t");
		}
		System.out.println("| Solutions");
		for(int a = 0; a<numRow; a++)
		{
			for(int b = 0; b<numCol; b++)
			{
				System.out.print((int)matrix[a][b] + "\t");
			}
			
			System.out.println("| " + solutions.get(a));
		}			
	}
	private void createLU() //modifies 'matrix' to U and makes L
	{
		L = new double[numRow][numRow];
		for(int i = 0; i < numRow; i++) //creates diagonal of 1
			L[i][i] = 1;
		for(int b = 1; b < numRow; b++)
		{
			for(int a = 0; a < b; a++)
			{
				 compareRow(a,b);
			}
		}
	}
	public ArrayList<Double> solveLYb()
	{
		//Uy = b
		createLU();
		ArrayList<Double>ySolutions = new ArrayList<Double>();
		for(int i = 0; i < numRow; i++)
		{
			for(int j = 0; j < i; j++)
			{
				solutions.set(i,solutions.get(i)-L[i][j]*ySolutions.get(j));
			}
			double h = solutions.get(i);
			ySolutions.add(h);
		}
		//System.out.println(ySolutions);
		return ySolutions;
	}
	public void solve(ArrayList<Double>ySol) //input L and y solutions
	{	
		if(numRow != numCol)
		{
			System.out.println("Cannot compute non-square systems");
			return;
		}
		ArrayList<Double>backwards= new ArrayList<Double>();
		ArrayList<Double>inOrder= new ArrayList<Double>();
		for(int i = numRow-1; i >= 0; i--)
		{
			for(int j = numCol-1; j>i; j--)
			{
				solutions.set(i,solutions.get(i)-matrix[i][j]*backwards.get(numRow-1-j));
				//System.out.println(solutions.get(i));
			}
			double h = 0;
			if(matrix[i][i]!=0)
				h = solutions.get(i)/matrix[i][i];
			backwards.add(h);
		}
		for(int l = backwards.size()-1; l>=0; l--)
			inOrder.add(backwards.get(l));
		for(int m = 0; m < inOrder.size(); m++)
		{
			vars.add(inOrder.get(m));	
		}
		if(isFine())
		{
			System.out.println("One possible set of answers:");
			for(int m = 0; m < inOrder.size(); m++)
			{
				System.out.println(variables.get(m) + " = " + inOrder.get(m));
			}
		}
		else
			System.out.println("No solutions exist");
		
	}
	private void compareRow(int row1, int row2) //modifies L and adds rows to make U
	{
		//modifies [row1][row2] in U to 0 and L
		double what = -matrix[row2][row1]/matrix[row1][row1];
		for(int i =0; i < numCol; i++)
			matrix[row2][i] += matrix[row1][i]*what;
		L[row2][row1] = -what;
	}
	public void check()
	{
		for(int i = 0; i < numRow; i++)
			for(int j= 0; j < numCol; j++)
				System.out.println("Matrix@[" + i + "][" + j + "] is " + matrix[i][j]);
	}
	public void checkL()
	{
		for(int a = 0; a<numRow; a++)
		{
			for(int b = 0; b<numRow; b++)
			{
				System.out.print(L[a][b] + "\t");
			}
			
			System.out.println("|");
		}		
		for(int i = 0; i < numRow; i++)
			for(int j= 0; j < numCol; j++)
				System.out.println("Matrix@[" + i + "][" + j + "] is " + L[i][j]);
	}
	private boolean isFine()
	{
		for(int a = 0; a < vars.size(); a++)
			if(vars.get(a) == Double.NaN)
				return false;
		for(int i = 0; i < numRow; i++)
		{
			double testNum = 0;
			for(int j = 0; j < numCol; j++)
				testNum += testMatrix[i][j]*vars.get(j);
			if(Math.abs(testNum - testSolutions.get(i)) > 0.05)
				return false;
		}		
		return true;
	}
	@SuppressWarnings("resource")
	public static void main(String [] args)
	{
		System.out.println("Input Equations (enter 'done' to finish):");
		EquationSolver n = new EquationSolver();
		Scanner scan = new Scanner(System.in);
		boolean isDone = true;
		while(isDone)
		{
			String line = scan.nextLine();
			if(line.equals("done"))
				isDone = false;
			if(isDone)
			{
				n.addLine(line);
				System.out.println(line);
			}		
		}				
		n.createMatrix();
		n.printMatrix();
		n.solve(n.solveLYb());
	}
	/*public static void main(String[]args)
	{
		EquationSolver n = new EquationSolver();
		n.addLine("2x + 2y + 4z = 4");
		n.addLine("x + 3y + 2z = -2");
		n.addLine("3x + 4y + 2z = 0");		
		//System.out.println("#Rows: " + n.numRow + "\n#Col: " + n.numCol);
		n.createMatrix();
		n.solve(n.solveLYb());
		//n.printMatrix();
	}*/
}
