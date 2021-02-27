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
#include <CGAL/point_generators_2.h>
#include <CGAL/Random.h>
#include <CGAL/Polygon_2.h>
#include <cstdlib> 
#include <ctime>

using namespace std;


typedef CGAL::Exact_predicates_inexact_constructions_kernel            Kernel;
typedef CGAL::Projection_traits_xy_3<Kernel> Gt;
typedef CGAL::Triangulation_vertex_base_with_info_2< unsigned int, Gt > Vb;
typedef CGAL::Triangulation_data_structure_2<Vb>                       Tds;
typedef CGAL::Delaunay_triangulation_2<Gt, Tds>                    Delaunay;
typedef CGAL::Polygon_2<Kernel>                                     Polygon_2;

typedef Kernel::Point_2                                                  Point_2;

typedef Kernel::Iso_rectangle_2                                  Rectangle_2;
typedef std::vector<Point_2>                                        Container;
typedef CGAL::Random_points_in_square_2<Point_2>                  Point_i_generator;
typedef CGAL::Random_points_on_square_2<Point_2>                  Point_o_generator;

typedef Kernel::Point_3                                                Point;


double coord_coef;


class plane //a class that stores the crucial figures in terms of computing the orientation
{
public:
	static const int n = 3;			//the dimension of vectors
	const double ex = 2;			//we introduce the restriction of collinearity
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

	static double dot_product(double vector_line[], double direction[], int n = 3) //function that computes the dot product of vectors
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
				k += 1;
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

	void projection(double vector[], int n = 2) //function that projects the normal vector onto the horizontal plane
	{
		if (vector[2] < 0)
		{
			this->directional[0] = -1 * vector[0];
			this->directional[1] = -1 * vector[1];
		}
		else
		{
			this->directional[0] = vector[0];
			this->directional[1] = vector[1];
		}
	}

	void get_normal(double v1[], double v2[], int n = 3)//function that computes the normal vector
	{
		normal_vec[0] = v1[1] * v2[2] - v2[1] * v1[2];
		normal_vec[1] = v1[2] * v2[0] - v2[2] * v1[0];
		normal_vec[2] = v1[0] * v2[1] - v2[0] * v1[1];
	}


	static double length(double line_vector[], int n = 3) //function that computes the length of a vector
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
};


double angle_between(plane plane1, plane plane2) {

	double coeff = 180 / M_PI;
	double expression = abs(plane::dot_product(plane1.normal_vec, plane2.normal_vec)) / (plane::length(plane1.normal_vec)*plane::length(plane2.normal_vec));
	double angle = acos(expression);
	return angle * coeff;
	return angle;
}


int main()
{
	srand((unsigned)time(NULL));
	std::cout << "This is generator of faulted triangulated surfaces." << std::endl;

	int i = 1;

	while (i < 10000)
	{
		string file_path = "C:\\testy\\scikit7\\";
		file_path.append(to_string(i));
		file_path.append(".txt");

		ofstream saving(file_path);
		saving << "X_C;" << "Y_C;" << "Z_C;" << "XN;" << "YN;" << "ZN;" << "AngleNeighbor1;" << "AngleNeighbor2;" << "AngleNeighbor3;" <<
		 "n1_xn;"  << "n1_yn;" << "n1_zn;" << 
		 "n2_xn;" << "n2_yn;"  << "n2_zn;" <<
		  "n3_xn;" << "n3_yn;" << "n3_zn;" <<
		   "Fault" << endl;


		vector<Point_2> points_f; //vector for storing faults

		Point_o_generator f(1); // faults (f), argument denotes the size of a square

		std::copy_n(f, 4, back_inserter(points_f));

		//cout << "Duplikat w drugiej parze?" << ((points_f[0].x() == points_f[1].x()) || (points_f[0].y() == points_f[1].y())) << endl;
		if ((points_f[2].x() == points_f[3].x()) || (points_f[2].y() == points_f[3].y())) {
			double x1 = points_f[2].x();
			double y1 = points_f[2].y();

			points_f.erase(points_f.begin() + 2);
			points_f.insert(points_f.begin() + 2, Point_2(y1, x1));
		}

		if ((points_f[0].x() == points_f[1].x()) || (points_f[0].y() == points_f[1].y())) {
			double x3 = points_f[0].x();
			double y3 = points_f[0].y();

			points_f.erase(points_f.begin());
			points_f.insert(points_f.begin(), Point_2(y3, x3));
		}



		//cout << "Po zmianach:" << endl;
		//for (auto i : points_f) {
		//	cout << i << endl;
		//}

		//cout << "Teraz  2.5D" << endl;

		vector<Point_2> points_b; //vector for storing boreholes/points
		Point_i_generator b(1); // boreholes (b), argument denotes the size of a square

		std::copy_n(b, 100, back_inserter(points_b));

		vector<Point> points_b_inclined;
		for (auto it = points_b.begin(); it != points_b.end(); it++) {

			points_b_inclined.push_back(Point((*it).x(), (*it).y(), 0.00));//*0.03*((float)rand() / RAND_MAX)));
		}

		//for (auto i : points_b_inclined) {
			//cout << i << endl;
		//}

		vector<Point> points_b_inclined_mod;

		for (auto it = points_b_inclined.begin(); it != points_b_inclined.end(); it++)
		{
			
			switch (CGAL::orientation(
				Point_2(points_f[0].x(), points_f[0].y()),
				Point_2(points_f[1].x(), points_f[1].y()),
				Point_2((*it).x(), (*it).y()))
				) 
			{
			case CGAL::LEFT_TURN:													//zamiast 0.10 by³ 0.03
				points_b_inclined_mod.push_back(Point((*it).x(), (*it).y(), (*it).z() - (0.10+static_cast <float> (rand()) / (static_cast <float> (RAND_MAX / (0.07 - 0.03)))))); break;
			case CGAL::RIGHT_TURN:
				points_b_inclined_mod.push_back(Point((*it).x(), (*it).y(), (*it).z() + (0.10+static_cast <float> (rand()) / (static_cast <float> (RAND_MAX / (0.07 - 0.03)))))); break;
			case CGAL::COLLINEAR:
				points_b_inclined_mod.push_back(Point((*it).x(), (*it).y(), -9999999)); break; //+static_cast <float> (rand()) / (static_cast <float> (RAND_MAX / (0.07 - 0.03)))))); break;
			}
		}
		/*
		for (auto it = points_b_inclined.begin(); it != points_b_inclined.end(); it++) {
			
			
			switch (CGAL::orientation(
				Point_2(points_f[2].x(), points_f[2].y()),
				Point_2(points_f[3].x(), points_f[3].y()),
				Point_2((*it).x(), (*it).y()))
				)
			{
			case CGAL::LEFT_TURN:													//zamiast 0.10 by³ 0.03
				points_b_inclined_mod.push_back(Point((*it).x(), (*it).y(), (*it).z() - (0.10))); break; //+static_cast <float> (rand()) / (static_cast <float> (RAND_MAX / (0.07 - 0.03)))))); break;
			case CGAL::RIGHT_TURN:
				points_b_inclined_mod.push_back(Point((*it).x(), (*it).y(), (*it).z() + (0.10))); break; //+static_cast <float> (rand()) / (static_cast <float> (RAND_MAX / (0.07 - 0.03)))))); break;
			case CGAL::COLLINEAR:
				points_b_inclined_mod.push_back(Point((*it).x(), (*it).y(), -9999999)); break; //+static_cast <float> (rand()) / (static_cast <float> (RAND_MAX / (0.07 - 0.03)))))); break;

			}
		
		}
		*/
		Delaunay dt;

		dt.insert(points_b_inclined_mod.begin(), points_b_inclined_mod.end());
		double point_1[3];
		double point_2[3];
		double point_3[3];

		double n1_point_1[3];
		double n1_point_2[3];
		double n1_point_3[3];

		double n2_point_1[3];
		double n2_point_2[3];
		double n2_point_3[3];

		double n3_point_1[3];
		double n3_point_2[3];
		double n3_point_3[3];

		Kernel::Line_2 f1(Point_2(points_f[0].x(), points_f[0].y()), Point_2(points_f[1].x(), points_f[1].y()));
		//Kernel::Line_2 f2(Point_2(points_f[2].x(), points_f[2].y()), Point_2(points_f[3].x(), points_f[3].y()));


		for (Delaunay::Finite_faces_iterator fit = dt.finite_faces_begin(); fit != dt.finite_faces_end(); ++fit) //a loop for performing the Delaunay triangulation and save the results

		{
			Delaunay::Face_handle face = fit;
			point_1[0] = dt.triangle(face)[0][0]; //extracting coordinates of points building a Delaunay triangle
			point_1[1] = dt.triangle(face)[0][1];
			point_1[2] = dt.triangle(face)[0][2];


			point_2[0] = dt.triangle(face)[1][0];
			point_2[1] = dt.triangle(face)[1][1];
			point_2[2] = dt.triangle(face)[1][2];


			point_3[0] = dt.triangle(face)[2][0];
			point_3[1] = dt.triangle(face)[2][1];
			point_3[2] = dt.triangle(face)[2][2];

			Point_2 t1(dt.triangle(face)[0][0], //extracting coordinates of points building a Delaunay triangle
				dt.triangle(face)[0][1]);

			Point_2 t2(dt.triangle(face)[1][0],
				dt.triangle(face)[1][1]);

			Point_2 t3(dt.triangle(face)[2][0],
				dt.triangle(face)[2][1]);

			plane current_plane = plane(point_1, point_2, point_3);					//constructing a plane that is processed at the moment
			string result = current_plane.measure();								//extracting the dip angle and the dip direction
			vector<string> centroid = current_plane.center(point_1, point_2, point_3);		//extracting the centroid of a Delaunay triangle

			double x_n = current_plane.normal_vec[0]; //extracting coordinates of the normal vector of a planar Delaunay triangle
			double y_n = current_plane.normal_vec[1];
			double z_n = current_plane.normal_vec[2];

			x_n = x_n / current_plane.length(current_plane.normal_vec);
			y_n = y_n / current_plane.length(current_plane.normal_vec);
			z_n = z_n / current_plane.length(current_plane.normal_vec);

			double x_d = current_plane.dip_vec[0]; //extracting coordinates of the dip vector of a planar Delaunay triangle
			double y_d = current_plane.dip_vec[1];
			double z_d = current_plane.dip_vec[2];

			//Wektory normalne s¹siadów

			double n1_x_n; //extracting coordinates of the normal vector of a planar Delaunay triangle
			double n1_y_n;
			double n1_z_n;

			double n2_x_n;//extracting coordinates of the normal vector of a planar Delaunay triangle
			double n2_y_n;
			double n2_z_n;

			double n3_x_n; //extracting coordinates of the normal vector of a planar Delaunay triangle
			double n3_y_n;
			double n3_z_n;
			


			//pierwszy sasiad
			double n1dist;

			if (dt.is_infinite(face->neighbor(0)) == true) {
				n1dist = -999999;

				n1_x_n = 0; //extracting coordinates of the normal vector of a planar Delaunay triangle
				n1_y_n = 0;
				n1_z_n = 0;
			}
			else {
				n1_point_1[0] = dt.triangle(face->neighbor(0))[0][0]; //extracting coordinates of points building a Delaunay triangle
				n1_point_1[1] = dt.triangle(face->neighbor(0))[0][1];
				n1_point_1[2] = dt.triangle(face->neighbor(0))[0][2];


				n1_point_2[0] = dt.triangle(face->neighbor(0))[1][0]; //extracting coordinates of points building a Delaunay triangle
				n1_point_2[1] = dt.triangle(face->neighbor(0))[1][1];
				n1_point_2[2] = dt.triangle(face->neighbor(0))[1][2];


				n1_point_3[0] = dt.triangle(face->neighbor(0))[2][0]; //extracting coordinates of points building a Delaunay triangle
				n1_point_3[1] = dt.triangle(face->neighbor(0))[2][1];
				n1_point_3[2] = dt.triangle(face->neighbor(0))[2][2];

				plane neighbor1_plane = plane(n1_point_1, n1_point_2, n1_point_3);
				n1_x_n = neighbor1_plane.normal_vec[0] / neighbor1_plane.length(neighbor1_plane.normal_vec); //extracting coordinates of the normal vector of a planar Delaunay triangle
				n1_y_n = neighbor1_plane.normal_vec[1] / neighbor1_plane.length(neighbor1_plane.normal_vec);
				n1_z_n = neighbor1_plane.normal_vec[2] / neighbor1_plane.length(neighbor1_plane.normal_vec);

				n1dist = angle_between(current_plane, neighbor1_plane);
			}


			//2 sasiad
			double n2dist;

			if (dt.is_infinite(face->neighbor(1)) == true) {
				n2dist = -99999999;

				n2_x_n = 0; //extracting coordinates of the normal vector of a planar Delaunay triangle
				n2_y_n = 0;
				n2_z_n = 0;
			}
			else {
				n2_point_1[0] = dt.triangle(face->neighbor(1))[0][0]; //extracting coordinates of points building a Delaunay triangle
				n2_point_1[1] = dt.triangle(face->neighbor(1))[0][1];
				n2_point_1[2] = dt.triangle(face->neighbor(1))[0][2];


				n2_point_2[0] = dt.triangle(face->neighbor(1))[1][0]; //extracting coordinates of points building a Delaunay triangle
				n2_point_2[1] = dt.triangle(face->neighbor(1))[1][1];
				n2_point_2[2] = dt.triangle(face->neighbor(1))[1][2];


				n2_point_3[0] = dt.triangle(face->neighbor(1))[2][0]; //extracting coordinates of points building a Delaunay triangle
				n2_point_3[1] = dt.triangle(face->neighbor(1))[2][1];
				n2_point_3[2] = dt.triangle(face->neighbor(1))[2][2];

				plane neighbor2_plane = plane(n2_point_1, n2_point_2, n2_point_3);

				n2_x_n = neighbor2_plane.normal_vec[0] / neighbor2_plane.length(neighbor2_plane.normal_vec); //extracting coordinates of the normal vector of a planar Delaunay triangle
				n2_y_n = neighbor2_plane.normal_vec[1] / neighbor2_plane.length(neighbor2_plane.normal_vec);
				n2_z_n = neighbor2_plane.normal_vec[2] / neighbor2_plane.length(neighbor2_plane.normal_vec);

				n2dist = angle_between(current_plane, neighbor2_plane);
			}


			//3 sasiad

			double n3dist;


		
			if (dt.is_infinite(face->neighbor(2)) == true) {
				n3dist = -999999999;

				n3_x_n = 0; //extracting coordinates of the normal vector of a planar Delaunay triangle
				n3_y_n = 0;
				n3_z_n = 0;
			}
			else {
				n3_point_1[0] = dt.triangle(face->neighbor(2))[0][0]; //extracting coordinates of points building a Delaunay triangle
				n3_point_1[1] = dt.triangle(face->neighbor(2))[0][1];
				n3_point_1[2] = dt.triangle(face->neighbor(2))[0][2];

				n3_point_2[0] = dt.triangle(face->neighbor(2))[1][0]; //extracting coordinates of points building a Delaunay triangle
				n3_point_2[1] = dt.triangle(face->neighbor(2))[1][1];
				n3_point_2[2] = dt.triangle(face->neighbor(2))[1][2];

				n3_point_3[0] = dt.triangle(face->neighbor(2))[2][0]; //extracting coordinates of points building a Delaunay triangle
				n3_point_3[1] = dt.triangle(face->neighbor(2))[2][1];
				n3_point_3[2] = dt.triangle(face->neighbor(2))[2][2];

				plane neighbor3_plane = plane(n3_point_1, n3_point_2, n3_point_3);

				n3_x_n = neighbor3_plane.normal_vec[0]/ neighbor3_plane.length(neighbor3_plane.normal_vec) ; //extracting coordinates of the normal vector of a planar Delaunay triangle
				n3_y_n = neighbor3_plane.normal_vec[1]/ neighbor3_plane.length(neighbor3_plane.normal_vec);
				n3_z_n = neighbor3_plane.normal_vec[2]/ neighbor3_plane.length(neighbor3_plane.normal_vec);


				n3dist = angle_between(current_plane, neighbor3_plane);
			}
			
			
			bool intersect = CGAL::do_intersect(Kernel::Triangle_2(t1, t2, t3), f1); //|| CGAL::do_intersect(Kernel::Triangle_2(t1, t2, t3), f2);

			    saving << 
				centroid[0] << ";" << centroid[1] << ";" << centroid[2] << ";" << 
				x_n << ";" << y_n << ";" << z_n << ";" <<
				n1dist << ";" << n2dist << ";" << n3dist << ";" <<
					n1_x_n << ";" << n1_y_n << ";" << n1_z_n << ";" <<
					n2_x_n << ";" << n2_y_n << ";" << n2_z_n << ";" <<
					n3_x_n << ";" << n3_y_n << ";" << n3_z_n << ";" <<
			    intersect << endl;
		}

		i++;
	}


	system("pause");
	return 0;
}
