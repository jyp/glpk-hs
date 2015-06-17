#include <glpk.h>
// #include <stdio.h>
// #include <stdlib.h>

glp_prob *c_glp_create_prob(){
  	glp_prob *lp;
	lp = glp_create_prob();
	return lp;
}

void c_glp_set_obj_name(glp_prob *lp, const char *name){
  	glp_set_obj_name(lp, name);
}

void c_glp_maximize(glp_prob *lp){
  	glp_set_obj_dir(lp, GLP_MAX);
}

void c_glp_minimize(glp_prob *lp){
  	glp_set_obj_dir(lp, GLP_MIN);
}

// void c_glp_set_obj_dir(glp_prob *lp, int dir){
//   	glp_set_obj_dir(lp, dir);
// }

int c_glp_get_bad_ray(glp_prob *lp){
  	return glp_get_unbnd_ray(lp);
}

int c_glp_add_rows(glp_prob *lp, int nrows){
  	return glp_add_rows(lp, nrows);
}

int c_glp_add_cols(glp_prob *lp, int ncols){
  	return glp_add_cols(lp, ncols);
}

void c_glp_set_obj_coef(glp_prob *lp, int j, double coef){
  	glp_set_obj_coef(lp, j, coef);
}

void c_glp_set_row_name(glp_prob *lp, int i, const char * name){
  	glp_set_row_name(lp, i, name);
}

void c_glp_set_col_name(glp_prob *lp, int i, const char * name){
	glp_set_col_name(lp, i, name);
}

void c_glp_set_row_bnds(glp_prob *lp, int i, int type, double lb, double ub){
	glp_set_row_bnds(lp, i, type, lb, ub);
}

void c_glp_set_col_bnds(glp_prob *lp, int i, int type, double lb, double ub){
	glp_set_col_bnds(lp, i, type, lb, ub);
}

void c_glp_set_mat_row(glp_prob *lp, int i, int len, const int ind[], const double val[]){
	glp_set_mat_row(lp, i, len, ind, val);
}

void c_glp_delete_prob(glp_prob *lp){
  	glp_delete_prob(lp);
}

void c_glp_create_index(glp_prob *lp){
  	glp_create_index(lp);
}

int c_glp_find_row(glp_prob *lp, const char *name){
  	return glp_find_row(lp, name);
}

int c_glp_find_col(glp_prob *lp, const char *name){
  	return glp_find_col(lp, name);
}

int c_glp_solve_simplex(glp_prob *lp, int msg_lev, int tm_lim, int presolve){
	glp_smcp smcp;
	glp_init_smcp (&smcp);
	smcp.msg_lev = msg_lev;
	smcp.tm_lim = tm_lim;
	smcp.presolve = presolve ? GLP_ON : GLP_OFF;
	glp_adv_basis(lp, 0);
	return glp_simplex(lp, &smcp);
}

double c_glp_get_obj_val(glp_prob *lp){
  	return glp_get_obj_val(lp);
}

double c_glp_get_row_prim(glp_prob *lp, int i){
  	return glp_get_row_prim(lp, i);
}

double c_glp_get_col_prim(glp_prob *lp, int i){
  	return glp_get_col_prim(lp, i);
}

void c_glp_set_col_kind(glp_prob *lp, int j, int kind){
	glp_set_col_kind(lp, j, kind);
}

int c_glp_mip_solve(glp_prob *lp, int msg_lev, int br_tech, int bt_tech, int pp_tech,
		     	int fp_heur, int tm_lim, int cuts, double mip_gap, int presolve){
  	glp_iocp iocp;
	glp_mem_limit(10000);
// 	printf ("%d %d %d time\n", msg_lev, br_tech, tm_lim);
	glp_init_iocp(&iocp);
	iocp.msg_lev = msg_lev;
	iocp.br_tech = br_tech;
	iocp.bt_tech = bt_tech;
	iocp.pp_tech = pp_tech;
	iocp.fp_heur = fp_heur ? GLP_ON : GLP_OFF;
// 	printf ("fp %d\n", iocp.fp_heur);
	iocp.gmi_cuts = cuts & 1 ? GLP_ON : GLP_OFF;
	iocp.mir_cuts = cuts & 2 ? GLP_ON : GLP_OFF;
	iocp.cov_cuts = cuts & 4 ? GLP_ON : GLP_OFF;
	iocp.clq_cuts = cuts & 8 ? GLP_ON : GLP_OFF;
	iocp.mip_gap = mip_gap;
	iocp.tm_lim = tm_lim;
// 	printf ("%d %d %d time\n", msg_lev, br_tech, tm_lim);
	iocp.presolve = presolve ? GLP_ON : GLP_OFF;
	return glp_intopt(lp, &iocp);
}

double c_glp_mip_obj_val (glp_prob *mip){
  	return glp_mip_obj_val(mip);
}

double c_glp_mip_row_val (glp_prob *mip, int i){
  	return glp_mip_row_val(mip, i);
}

double c_glp_mip_col_val (glp_prob *mip, int j){
  	return glp_mip_col_val(mip, j);
}

int c_glp_get_obj_dir (glp_prob *lp){
  	return glp_get_obj_dir (lp);
}

int c_glp_get_num_rows (glp_prob *lp){
  	return glp_get_num_rows (lp);
}

int c_glp_get_num_cols (glp_prob *lp){
 	return glp_get_num_cols (lp);
}

const char *c_glp_get_row_name (glp_prob *lp, int i){
  	return glp_get_row_name (lp, i);
}

const char *c_glp_get_col_name (glp_prob *lp, int i){
  	return glp_get_col_name (lp, i);
}

int c_glp_get_col_kind (glp_prob *lp, int i){
  	return glp_get_col_kind (lp, i);
}

// double c_dbl_max(){
//   	return DBL_MAX;
// }

double c_glp_get_row_lb (glp_prob *lp, int i){
	return glp_get_row_lb (lp, i);
}

double c_glp_get_row_ub (glp_prob *lp, int i){
	return glp_get_row_ub (lp, i);
}

double c_glp_get_col_lb (glp_prob *lp, int i){
	return glp_get_col_lb (lp, i);
}

double c_glp_get_col_ub (glp_prob *lp, int i){
	return glp_get_col_ub (lp, i);
}

int c_glp_get_row_type (glp_prob *lp, int i){
  	return glp_get_row_type(lp, i);
}

int c_glp_get_col_type (glp_prob *lp, int i){
  	return glp_get_col_type(lp, i);
}

double c_glp_get_obj_coef (glp_prob *lp, int j){
  	return glp_get_obj_coef(lp, j);
}

int c_glp_get_mat_row (glp_prob *lp, int i, int ind[], double val[]){
  	return glp_get_mat_row (lp, i, ind, val);
}

int c_glp_read_lp(glp_prob *lp, const char * fname){
  	return glp_read_lp(lp, 0, fname);
}

int c_glp_write_lp(glp_prob *lp, const char * fname){
  	return glp_write_lp(lp, 0, fname);
}