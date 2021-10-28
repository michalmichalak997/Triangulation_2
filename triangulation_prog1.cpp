//   GeoAnomalia, a program to calculate local geometry of geological surfaces. 
//   As input you need points (XYZ) documenting  surface. As a result you will get orientations (dip and dip direction) attached to Delaunay triangles.
//   The output of this program can serve as an input for unsupervised learning approaches (e.g. you can cluster normal vectors to the triangles or dip vectors of the triangles) 
//   Copyright © 2020, 2021 Michał Michalak
//   Copyright © 2016 CGAL
//   This program is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.

//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.

//    You should have received a copy of the GNU General Public License
//   along with this program.  If not, see <http://www.gnu.org/licenses/>
//   Contact: michalmichalak@us.edu.pl, michalm@agh.edu.pl

#include <fstream>
#include <sstream>
#include <string>
#define _USE_MATH_DEFINES
#include <math.h>
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Projection_traits_xy_3.h>
#include <CGAL/Delaunay_triangulation_2.h>
#include <CGAL/Triangulation_2.h>
#include <CGAL/Triangulation_vertex_base_with_info_2.h>
#include <vector>
#include <random>


using namespace std;


typedef CGAL::Exact_predicates_inexact_constructions_kernel            Kernel;
typedef CGAL::Projection_traits_xy_3<Kernel> Gt;
typedef CGAL::Triangulation_vertex_base_with_info_2< unsigned int, Gt > Vb;
typedef CGAL::Triangulation_data_structure_2<Vb>                       Tds;
typedef CGAL::Delaunay_triangulation_2<Gt, Tds>                    Delaunay;

typedef Kernel::Point_3                                                Point;


double coord_coef; //a ratio to scale coordinates
const int n = 3; //we work in 3D
const double ex = 2; //we introduce the restriction of collinearity


class plane //a class that stores the crucial figures in terms of computing the orientation
{

private:

	double first_vec[n];            //the first edge of a triangle
	double second_vec[n];			//the second edge of a triangle
	double third_vec[n];			//the third edge of a triangle
	double normal_vec[n];			//normal vector of a triangle
	double directional[n];			//the projection of the normal vector onto the horizontal plane
	double z_axis[n] = { 0,0,1 };   //the definition of the z-axis
	double dip_vec[n];
	double doc;						//a variable that contains the collinearity coefficient
	double area;					//a variable that stores the area of a triangle
	bool lin_dependence;		    //a bool variable to check to answer whether points are (too) collinear
	string dip_degrees;             //a text variable to store the dip angle
	string azimuth_degrees;         //a text variable to store the dip direction


public:
	double dip_azimuth(double normal[], int n = 2) //a function that computes the dip azimuth
	{
		double coeff = 180 / M_PI;
		double angle = atan2(normal[1], normal[0]);
		angle = angle * coeff;
		if (angle < 0)
		{
			return angle + 360;
		}
		else
		{
			return angle;
		}
	}

	double dip_angle(double z_axis[], double normal_v[]) //function that computes the dip angle
	{
		double angle;
		double expression;
		double coeff = 180 / M_PI;
		expression = abs(dot_product(z_axis, normal_v)) / (length(z_axis)*length(normal_v));
		angle = acos(expression);
		return angle * coeff;
	}

	double dot_product(double vector_line[], double direction[], int n = 3) //function that computes the dot product of vectors
	{
		double product = 0;
		for (int i = 0; i < n; i++)
		{
			product += direction[i] * vector_line[i];
		}
		return product;
	}

	bool dependence(double v1[], double v2[], double v3[]) //function that checks whether the points are collinear
	{
		double len_v1 = length(v1);
		double len_v2 = length(v2);
		double len_v3 = length(v3);
		double lengths[n] = { len_v1, len_v2, len_v3 };

		sort(lengths, lengths + n);
		this->doc = lengths[2] / (lengths[0] + lengths[1]);
		int k = 0;
		for (int i = 0; i < n; i++)
		{
			if (lengths[i] == 0)
			{
				//k += 1;
				throw runtime_error("Points coincidence");

			}
		}
		if (k != 0)
		{
			return true;
		}
		else
		{
			if (doc > ex)
			{
				return true;
			}
			else
			{
				return false;
			}
		}
	}

	//void projection(double vector[], int n = 2) //function that projects the normal vector onto the horizontal plane
	//{
	//	if (vector[2] < 0)
	//	{
	//		this->directional[0] = -1 * vector[0];
	//		this->directional[1] = -1 * vector[1];
	//	}
	//	else
	//	{
	//		this->directional[0] = vector[0];
	//		this->directional[1] = vector[1];
	//	}
	//}


	double length(double line_vector[], int n = 3) //function that computes the length of a vector
	{
		double vector_length = sqrt(pow(line_vector[0], 2) + pow(line_vector[1], 2) + pow(line_vector[2], 2));
		return vector_length;
	}


	vector <string> center(double point_1[], double point_2[], double point_3[]) //function that computes the geometrical centre of a triangle
	{
		double x = (point_1[0] + point_2[0] + point_3[0]) / (3.0);
		double y = (point_1[1] + point_2[1] + point_3[1]) / (3.0);
		double z = (point_1[2] + point_2[2] + point_3[2]) / (3.0);

		vector<string> napis{ to_string(x), to_string(y), to_string(z) };
		return napis;
	}

	plane(double point_1[], double point_2[], double point_3[]) //the class constructor
	{
		double coeff = 180 / M_PI;
		double first_try[n];
		double second_try[n];
		double third_try[n];

		for (int i = 0; i < n; i++)
		{
			first_try[i] = point_2[i] - point_1[i];
			second_try[i] = point_3[i] - point_1[i];
			third_try[i] = point_3[i] - point_2[i];
		}

		bool test = dependence(first_try, second_try, third_try);
		if (test == true)
		{
			lin_dependence = true;
		}
		else
		{
			lin_dependence = false;
			for (int i = 0; i < n; i++)
			{
				this->first_vec[i] = first_try[i];
				this->second_vec[i] = second_try[i];
				this->third_vec[i] = third_try[i];
			}
			normal_vec[0] = first_vec[1] * second_vec[2] - second_vec[1] * first_vec[2];
			normal_vec[1] = first_vec[2] * second_vec[0] - second_vec[2] * first_vec[0];
			normal_vec[2] = first_vec[0] * second_vec[1] - second_vec[0] * first_vec[1];

			if (normal_vec[2] < 0) {
				normal_vec[0] *= -1;
				normal_vec[1] *= -1;
				normal_vec[2] *= -1;
			}
			double normal_vector_length = length(normal_vec);

			normal_vec[0] /= normal_vector_length;
			normal_vec[1] /= normal_vector_length;
			normal_vec[2] /= normal_vector_length;

			this->dip_vec[0] = cos(dip_angle(z_axis, normal_vec) / coeff)*cos(dip_azimuth(normal_vec) / coeff);
			this->dip_vec[1] = cos(dip_angle(z_axis, normal_vec) / coeff)*sin(dip_azimuth(normal_vec) / coeff);
			this->dip_vec[2] = -sin(dip_angle(z_axis, normal_vec) / coeff);

			double stala = 0.5;
			double half = stala * (length(first_vec) + length(second_vec) + length(third_vec));
			double s = sqrt(half*(half - length(first_vec))*(half - length(second_vec))*(half - length(third_vec)));
			this->area = s * 0.0001;
		}
	}

	string measure()//function that supplies orientation results also for singularities
	{
		if (lin_dependence)
		{
			azimuth_degrees = ("LT");
			dip_degrees = ("LT");
			return (dip_degrees + ";" + azimuth_degrees);
		}
		else if (normal_vec[0] == 0 && normal_vec[1] == 0 && normal_vec[2] != 0)
		{
			dip_degrees = "0";
			azimuth_degrees = ("x");
			return (dip_degrees + ";" + azimuth_degrees);
		}
		else if (normal_vec[2] == 0)
		{
			dip_degrees = "90";
			azimuth_degrees = to_string(dip_azimuth(normal_vec));
			return dip_degrees + ";" + azimuth_degrees;
		}
		else
		{
			double dipping_angle = dip_angle(z_axis, normal_vec);
			dip_degrees = to_string(dipping_angle);
			azimuth_degrees = to_string(dip_azimuth(normal_vec));
			return dip_degrees + ";" + azimuth_degrees;
		}
	}

	vector<double> get_normal() //function that -computes- returns the normal vector
	{

		vector<double> normal_vector = { normal_vec[0] ,normal_vec[1], normal_vec[2] };

		return normal_vector;
	}

	double get_area() {

		return(area);
	}

	double get_doc() {

		return(doc);
	}

	vector<double> get_dip_vec() {

		vector<double> dip_vector = { dip_vec[0],dip_vec[1] ,dip_vec[2] };
		return(dip_vector);
	}
};


int main()
{
	string path_i, path_o, path_del, normals, gridpath, path_gridvis; //text variables for input and output paths, respectively
	double resolution_step; //the density of the grid map

	std::cout << "Type in the path of your input data:" << endl; //the user is required to type in the input path
	std::cout << "Example: C:\\dev\\CGAL-4.8\\examples\\Triangulation_2\\JurassicBottomInput.txt" << endl << endl;

	std::cin >> path_i;

	std::cout << "What is the coordinates coefficient - the ratio by which coordinates were multiplied to get integers (e.g. 100.00, 1000.00)" << endl;
	std::cin >> ::coord_coef;

	ifstream download(path_i);

	if (!download) std::cout << "Error in opening file" << endl; //the case when the file cannot be uploaded

	string tempor;//a temporary variable storing figures while uploading

	vector< std::pair<Point, unsigned> > pts; //a variable storing points

	while (getline(download, tempor))//loading points line-by-line
	{
		istringstream convert(tempor);
		double a, b, c;
		unsigned int d;

		if (!(convert >> a >> b >> c >> d)) { break; }
		pts.push_back(make_pair(Point(a / coord_coef, b / coord_coef, c / coord_coef), d));
	}


	double min_x = pts.begin()->first[0];
	double min_y = pts.begin()->first[1];

	double max_x = pts.begin()->first[0];
	double max_y = pts.begin()->first[1];

	for (auto it = pts.begin(); it != pts.end(); it++) { //calculating boundary coordinates

		if (it->first[0] < min_x) {
			min_x = it->first[0];
		}

		if (it->first[1] < min_y) {
			min_y = it->first[1];
		}

		if (it->first[0] > max_x) {
			max_x = it->first[0];
		}

		if (it->first[1] > max_y) {
			max_y = it->first[1];
		}

	}



	std::cout << "Type in the path of the output:" << endl; //the user is required to type in the output path
	std::cout << "Example: C:\\dev\\CGAL-4.8\\examples\\Triangulation_2\\JurassicBottomOutput.txt" << endl << endl;

	std::cin >> path_o;

	std::cout << "Type in the path of the Delaunay visualization .vtu file:" << endl; //the user is required to type in the output path
	std::cout << "Example: C:\\dev\\CGAL-4.8\\examples\\Triangulation_2\\Delaunay.vtu" << endl << endl;

	std::cin >> path_del;

	std::cout << "Type in the path of the normals .vtu file:" << endl; //the user is required to type in the output path
	std::cout << "Example: C:\\dev\\CGAL-4.8\\examples\\Triangulation_2\\normals.vtu" << endl << endl;

	std::cin >> normals;

	std::cout << "Type in the path of gridpath .txt file:" << endl; //the user is required to type in the output path
	std::cout << "Example: C:\\dev\\CGAL-4.8\\examples\\Triangulation_2\\gridpath.txt" << endl << endl;

	std::cin >> gridpath;

	std::cout << "Type in the path of grid .vtu file:" << endl; //the user is required to type in the output path
	std::cout << "Example: C:\\dev\\CGAL-4.8\\examples\\Triangulation_2\\gridvis.vtu" << endl << endl;

	std::cin >> path_gridvis;

	std::cout << "Type in the grid resolution: (e.g. 100.00)" << endl;
	std::cin >> resolution_step;

	Delaunay dt; //a variable storing the geometrical elements of Delaunay triangulation
	dt.insert(pts.begin(), pts.end());

	cout << "The number of points taken:" << pts.size() << ". The number of vertices in triangulation:" << dt.number_of_vertices() << endl;

	if (pts.size() != dt.number_of_vertices()) { throw(runtime_error("Check for duplicates in data!")); }

	ofstream saving(path_o); //a stream variable to save output figures

	saving << "X1;" << "Y1;" << "Z1;" << "X2;" << "Y2;" << "Z2;" << "X3;" << "Y3;" << "Z3;" << "X_C;" //column names are saved
		<< "Y_C;" << "Z_C;" << "X_N;" << "Y_N;" << "Z_N;" << "X_D;" << "Y_D;" << "Z_D;" << "Dip_ang;" << "Dip_dir;" << "DOC;" << "Area;" << "IDT1;" << "IDT2;" << "IDT3" << endl;

	ofstream gridsave(gridpath);

	vector<Point> grid_pts;
	double elevation_grid_pts = 0.00; //the z-coordinate is equal to zero (arbitrary decision)

	for (auto i = min_x; i < max_x; i = i + resolution_step) {
		for (auto j = min_y; j < max_y; j = j + resolution_step) {
			grid_pts.push_back(Point(i, j, elevation_grid_pts));
		}
	}

	gridsave << "px;" << "py;" << "IDT1;" << "IDT2;" << "IDT3" << endl;

	vector<Point> grid_pts_finite;

	for (auto regpoint : grid_pts) {
		Delaunay::Face_handle facereg = dt.locate(regpoint);


		int inf1 = facereg->vertex(0)->info();
		int inf2 = facereg->vertex(1)->info();
		int inf3 = facereg->vertex(2)->info();

		if ((inf1 > 0) & (inf2 > 0) & (inf3 > 0)) //we are interested only in points within the convex hull
		{
			grid_pts_finite.push_back(Point(regpoint.x(), regpoint.y(), elevation_grid_pts));
			gridsave << fixed << regpoint.x() << ";" << regpoint.y() << ";" << inf1 << ";" << inf2 << ";" << inf3 << endl;
		}
	}

	double point_1[4];
	double point_2[4];
	double point_3[4];

	for (Delaunay::Finite_faces_iterator fit = dt.finite_faces_begin(); fit != dt.finite_faces_end(); ++fit) //a loop for performing the Delaunay triangulation and save the results

	{
		Delaunay::Face_handle face = fit;
		point_1[0] = dt.triangle(face)[0][0]; //extracting coordinates of points building a Delaunay triangle
		point_1[1] = dt.triangle(face)[0][1];
		point_1[2] = dt.triangle(face)[0][2];
		point_1[3] = face->vertex(0)->info();

		point_2[0] = dt.triangle(face)[1][0];
		point_2[1] = dt.triangle(face)[1][1];
		point_2[2] = dt.triangle(face)[1][2];
		point_2[3] = face->vertex(1)->info();

		point_3[0] = dt.triangle(face)[2][0];
		point_3[1] = dt.triangle(face)[2][1];
		point_3[2] = dt.triangle(face)[2][2];
		point_3[3] = face->vertex(2)->info();

		plane current_plane = plane(point_1, point_2, point_3);					//constructing a plane that is processed at the moment
		string result = current_plane.measure();								//extracting the dip angle and the dip direction
		vector<string> centroid = current_plane.center(point_1, point_2, point_3);		//extracting the centroid of a Delaunay triangle

		double x_n = current_plane.get_normal()[0]; //extracting coordinates of the normal vector of a planar Delaunay triangle
		double y_n = current_plane.get_normal()[1];
		double z_n = current_plane.get_normal()[2];

		//double normal_vector_double[n] = { current_plane.get_normal()[0] ,current_plane.get_normal()[1] ,current_plane.get_normal()[2] };
		//x_n = x_n / current_plane.length(normal_vector_double);
		//y_n = y_n / current_plane.length(normal_vector_double);
		//z_n = z_n / current_plane.length(normal_vector_double);

		double x_d = current_plane.get_dip_vec()[0]; //extracting coordinates of the dip vector of a planar Delaunay triangle
		double y_d = current_plane.get_dip_vec()[1];
		double z_d = current_plane.get_dip_vec()[2];

		saving << to_string(point_1[0]) << ";" << to_string(point_1[1]) << ";" << to_string(point_1[2]) << ";" << //saving orientation elements with respect to the column names
			to_string(point_2[0]) << ";" << to_string(point_2[1]) << ";" << to_string(point_2[2]) << ";" <<
			to_string(point_3[0]) << ";" << to_string(point_3[1]) << ";" << to_string(point_3[2]) << ";" <<
			centroid[0] << ";" << centroid[1] << ";" << centroid[2] << ";" << x_n << ";" << y_n << ";" << z_n << ";"
			<< x_d << ";" << y_d << ";" << z_d << ";" << result << ";" << current_plane.get_doc() << ";" << current_plane.get_area() << ";" <<
			point_1[3] << ";" << point_2[3] << ";" << point_3[3] << endl;
	}

	ofstream delaunays(path_del);

	delaunays <<
		"<?xml version=\"1.0\"?>" << "\n" <<
		"<VTKFile type=\"UnstructuredGrid\" version=\"0.1\" byte_order=\"LittleEndian\" compressor=\"vtkZLibDataCompressor\">" << "\n  " <<
		"<UnstructuredGrid>" << "\n    " <<
		"<Piece NumberOfPoints=\"" << dt.number_of_vertices() << "\" NumberOfCells=\"" << dt.number_of_faces() << "\">" << "\n      "
		"<PointData Scalars=\"scalars\">" << "\n        "
		"<DataArray type=\"Float32\" Name=\"scalars\" format=\"ascii\">" << "\n          ";

	for (auto it = pts.begin(); it != pts.end(); it++)
	{
		delaunays << fixed << (it->first[2]) << "\n          ";
	}

	delaunays << "\n        " <<
		"</DataArray>" << "\n      " <<
		"</PointData>" << "\n      " <<
		"<Points>" << "\n        " <<
		"<DataArray type=\"Float32\" NumberOfComponents=\"3\" format=\"ascii\">" << "\n           ";

	for (auto it = pts.begin(); it != pts.end(); it++)
	{
		double x = (it->first[1]), y = (it->first[0]), z = (it->first[2]);
		delaunays << fixed << x << " " << y << " " << z << "\n           ";
	}

	delaunays << "\n        " <<
		"</DataArray>" << "\n      " <<
		"</Points>" << "\n      " <<
		"<Cells>" << "\n        " <<
		"<DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">" << "\n          ";

	for (Delaunay::Finite_faces_iterator fit = dt.finite_faces_begin(); fit != dt.finite_faces_end(); ++fit)
	{
		Delaunay::Face_handle face = fit;
		delaunays << face->vertex(0)->info() - 1 << " " << face->vertex(1)->info() - 1 << " " << face->vertex(2)->info() - 1 << "\n          ";
	}

	delaunays << "\n        " <<
		"</DataArray>" << "\n        " <<
		"<DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">" << "\n          ";

	for (auto i = 3; i <= 3 * dt.number_of_faces(); i = i + 3)
	{
		delaunays << i << " ";
	}

	delaunays << "\n        " <<
		"</DataArray>" << "\n        " <<
		"<DataArray type=\"UInt8\" Name=\"types\" format=\"ascii\">" <<
		"\n          ";

	for (auto i = 1; i <= dt.number_of_faces(); i++)
	{
		delaunays << 5 << " ";
	}

	delaunays << "\n        " <<
		"</DataArray>" << "\n      " <<
		"</Cells>" << "\n    " <<
		"</Piece>" << "\n  " <<
		"</UnstructuredGrid>" << "\n" <<
		"</VTKFile>";



	ofstream normalvis(normals);

	normalvis <<
		"<?xml version=\"1.0\"?>" << "\n" <<
		"<VTKFile type=\"UnstructuredGrid\" version=\"0.1\" byte_order=\"LittleEndian\" compressor=\"vtkZLibDataCompressor\">" << "\n  " <<
		"<UnstructuredGrid>" << "\n    " <<
		"<Piece NumberOfPoints=\"" << dt.number_of_faces() << "\" NumberOfCells=\"" << dt.number_of_faces() << "\">" << "\n      "

		"<PointData Scalars=\"scalars\">" << "\n        "
		"<DataArray type=\"Float32\" Name=\"scalars\" format=\"ascii\">" << "\n          ";


	for (Delaunay::Finite_faces_iterator fit = dt.finite_faces_begin(); fit != dt.finite_faces_end(); ++fit) //a loop for performing the Delaunay triangulation and save the results

	{
		Delaunay::Face_handle face = fit;
		point_1[0] = dt.triangle(face)[0][0]; //extracting coordinates of points building a Delaunay triangle
		point_1[1] = dt.triangle(face)[0][1];
		point_1[2] = dt.triangle(face)[0][2];
		point_1[3] = face->vertex(0)->info();

		point_2[0] = dt.triangle(face)[1][0];
		point_2[1] = dt.triangle(face)[1][1];
		point_2[2] = dt.triangle(face)[1][2];
		point_2[3] = face->vertex(1)->info();

		point_3[0] = dt.triangle(face)[2][0];
		point_3[1] = dt.triangle(face)[2][1];
		point_3[2] = dt.triangle(face)[2][2];
		point_3[3] = face->vertex(2)->info();

		plane current_plane = plane(point_1, point_2, point_3);					//constructing a plane that is processed at the moment
		vector<string> centroid = current_plane.center(point_1, point_2, point_3);		//extracting the centroid of a Delaunay triangle
		normalvis << fixed << centroid[2] << "\n          ";
	}

	normalvis << "\n        " <<
		"</DataArray>" << "\n      " <<
		"</PointData>" << "\n      " <<
		"<Points>" << "\n        " <<
		"<DataArray type=\"Float32\" NumberOfComponents=\"3\" format=\"ascii\">" << "\n           ";

	for (Delaunay::Finite_faces_iterator fit = dt.finite_faces_begin(); fit != dt.finite_faces_end(); ++fit) //a loop for performing the Delaunay triangulation and save the results

	{
		Delaunay::Face_handle face = fit;
		point_1[0] = dt.triangle(face)[0][0]; //extracting coordinates of points building a Delaunay triangle
		point_1[1] = dt.triangle(face)[0][1];
		point_1[2] = dt.triangle(face)[0][2];
		point_1[3] = face->vertex(0)->info();

		point_2[0] = dt.triangle(face)[1][0];
		point_2[1] = dt.triangle(face)[1][1];
		point_2[2] = dt.triangle(face)[1][2];
		point_2[3] = face->vertex(1)->info();

		point_3[0] = dt.triangle(face)[2][0];
		point_3[1] = dt.triangle(face)[2][1];
		point_3[2] = dt.triangle(face)[2][2];
		point_3[3] = face->vertex(2)->info();

		plane current_plane = plane(point_1, point_2, point_3);					//constructing a plane that is processed at the moment
		string result = current_plane.measure();								//extracting the dip angle and the dip direction
		vector <string> centroid = current_plane.center(point_1, point_2, point_3);

		normalvis << fixed << centroid[1] << " " << centroid[0] << " " << centroid[2] << "\n           ";

	}

	normalvis << "\n        " <<
		"</DataArray>" << "\n      " <<
		"</Points>" << "\n      " <<
		"<Cells>" << "\n        " <<
		"<DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">" << "\n          ";

	for (auto i = 0; i < dt.number_of_faces(); i++)
	{
		normalvis << i << "\n          ";
	}

	normalvis << "\n        " <<
		"</DataArray>" << "\n        " <<
		"<DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">" << "\n          ";

	for (auto i = 1; i <= dt.number_of_faces(); i++)
	{
		normalvis << i << " ";
	}

	normalvis << "\n        " <<
		"</DataArray>" << "\n        " <<
		"<DataArray type=\"UInt8\" Name=\"types\" format=\"ascii\">" <<
		"\n          ";

	for (auto i = 1; i <= dt.number_of_faces(); i++)
	{
		normalvis << 1 << " ";
	}

	normalvis << "\n        " <<
		"</DataArray>" << "\n      " <<
		"</Cells>" << "\n    " <<
		"<CellData Normals=\"cell_normals\">" << "\n      " <<
		"<DataArray type=\"Float32\" Name=\"cell_normals\" NumberOfComponents=\"3\" format=\"ascii\">" << "\n          ";


	for (Delaunay::Finite_faces_iterator fit = dt.finite_faces_begin(); fit != dt.finite_faces_end(); ++fit)
	{
		Delaunay::Face_handle face = fit;

		point_1[0] = dt.triangle(face)[0][0]; //extracting coordinates of points building a Delaunay triangle
		point_1[1] = dt.triangle(face)[0][1];
		point_1[2] = dt.triangle(face)[0][2];
		point_1[3] = face->vertex(0)->info();

		point_2[0] = dt.triangle(face)[1][0];
		point_2[1] = dt.triangle(face)[1][1];
		point_2[2] = dt.triangle(face)[1][2];
		point_2[3] = face->vertex(1)->info();

		point_3[0] = dt.triangle(face)[2][0];
		point_3[1] = dt.triangle(face)[2][1];
		point_3[2] = dt.triangle(face)[2][2];
		point_3[3] = face->vertex(2)->info();

		plane current_plane = plane(point_1, point_2, point_3);					//constructing a plane that is processed at the moment
		string result = current_plane.measure();								//extracting the dip angle and the dip direction
		vector <string> centroid = current_plane.center(point_1, point_2, point_3);		//extracting the centroid of a Delaunay triangle

		double x_n = current_plane.get_normal()[0]; //extracting coordinates of the normal vector of a planar Delaunay triangle
		double y_n = current_plane.get_normal()[1];
		double z_n = current_plane.get_normal()[2];

		//double normal_vector_double[n] = { current_plane.get_normal()[0] ,current_plane.get_normal()[1] ,current_plane.get_normal()[2] };
		//x_n = x_n / current_plane.length(normal_vector_double);
		//y_n = y_n / current_plane.length(normal_vector_double);
		//z_n = z_n / current_plane.length(normal_vector_double);

		normalvis << fixed << y_n << " " << x_n << " " << z_n << "\n          ";
	}

	normalvis <<
		"</DataArray>" << "\n      " <<
		"</CellData>" << "\n      " <<
		"<Cells>" << "\n        " <<
		"</Cells>" << "\n    " <<
		"</Piece>" << "\n  " <<
		"</UnstructuredGrid>" << "\n" <<
		"</VTKFile>";

	ofstream gridvis(path_gridvis);

	gridvis <<
		"<?xml version=\"1.0\"?>" << "\n" <<
		"<VTKFile type=\"UnstructuredGrid\" version=\"0.1\" byte_order=\"LittleEndian\" compressor=\"vtkZLibDataCompressor\">" << "\n  " <<
		"<UnstructuredGrid>" << "\n    " <<
		"<Piece NumberOfPoints=\"" << grid_pts_finite.size() << "\" NumberOfCells=\"" << grid_pts_finite.size() << "\">" << "\n      "
		"<PointData Scalars=\"scalars\">" << "\n        "
		"<DataArray type=\"Float32\" Name=\"scalars\" format=\"ascii\">" << "\n          ";

	for (auto it = grid_pts_finite.begin(); it != grid_pts_finite.end(); it++)
	{
		gridvis << it->z() << "\n          "; //the z-coordinate is equal to zero (arbitrary decision)
	}

	gridvis << "\n        " <<
		"</DataArray>" << "\n      " <<
		"</PointData>" << "\n      " <<
		"<Points>" << "\n        " <<
		"<DataArray type=\"Float32\" NumberOfComponents=\"3\" format=\"ascii\">" << "\n           ";

	for (auto it = grid_pts_finite.begin(); it != grid_pts_finite.end(); it++)
	{
		gridvis << fixed << it->y() << " " << it->x() << " " << it->z() << "\n           ";
	}

	gridvis << "\n        " <<
		"</DataArray>" << "\n      " <<
		"</Points>" << "\n      " <<
		"<Cells>" << "\n        " <<
		"<DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">" << "\n          ";


	for (auto i = 0; i < grid_pts_finite.size(); i++) //zamienic liczbe 24 na grid_pts size
	{
		gridvis << i << "\n          ";
	}

	gridvis << "\n        " <<
		"</DataArray>" << "\n        " <<
		"<DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">" << "\n          ";

	for (auto i = 1; i <= 1 * grid_pts_finite.size(); i = i + 1)
	{
		gridvis << i << " ";
	}

	gridvis << "\n        " <<
		"</DataArray>" << "\n        " <<
		"<DataArray type=\"UInt8\" Name=\"types\" format=\"ascii\">" <<
		"\n          ";

	for (auto i = 1; i <= grid_pts_finite.size(); i++)
	{
		gridvis << 1 << " ";
	}

	gridvis << "\n        " <<
		"</DataArray>" << "\n      " <<
		"</Cells>" << "\n    " <<
		"</Piece>" << "\n  " <<
		"</UnstructuredGrid>" << "\n" <<
		"</VTKFile>";

	std::system("pause");
	return 0;
}
