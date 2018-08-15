#include <Rcpp.h>
using namespace Rcpp;

// R Code
// evaluate <- function(g) {
//   e <- nrow(g$edges)
//   value <- 0
//   for (i in 1:(e-1)) {
//     for (j in (i+1):e) {
//     value <- value+intersect(g$vertices, g$map, g$edges[i,], g$edges[j,])
//    }
//   }
//   cnd <- !is.na(g$map)
//   px  <- g$vertices[cnd,]
//   mp  <- g$map[cnd]
//   qx  <- px
//   for (i in 1:nrow(px)) qx[mp[i],] <- px[i,]
//   pit <- pointsInTriangles(qx, g$faces)
//   vd  <- ifelse(pit>g$pts, 0, g$pts-pit)
//   c(intersection=value, vertdiff=sum(vd))
// }

//' Compute the number of points in trangles.
//' 
//' @param vertices numeric matrix with two columns: xy coordinates
//' @param faces numeric matrix with three columns: index of points which form the triangles
//' 
//' @export
//' @return a vector with number of points in each triangle
//' 
//' @references \url{https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle}
// [[Rcpp::export]]
NumericVector pointsInTrianglesC(NumericMatrix vertices, NumericMatrix faces) {
  int i, j, t1, t2, t3, pit;
  NumericVector ret(faces.nrow());
  double px, p0x, p1x, p2x, p3x, py, p0y, p1y, p2y, p3y, a,s, t;
  for (i=0; i<faces.nrow(); i++) {
    t1  = faces(i,0)-1; t2  = faces(i,1)-1; t3  = faces(i,2)-1;
    p0x = vertices(t1,0); p1x = vertices(t2,0); p2x = vertices(t3,0);
    p0y = vertices(t1,1); p1y = vertices(t2,1); p2y = vertices(t3,1);    
    //  pit <- function (px, p0x, p1x, p2x, py, p0y, p1y, p2y) {
    // https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
    //      a = (-p1y*p2x + p0y*(-p1x + p2x) + p0x*(p1y - p2y) + p1x*p2y);
    //      s = ((p0y*p2x - p0x*p2y + (p2y - p0y)*px + (p0x - p2x)*py))/a;
    //      t = ((p0x*p1y - p0y*p1x + (p0y - p1y)*px + (p1x - p0x)*py))/a;
    // #print(data.frame(s=s, t=t, st=s+t, l=(s>tol) & (t>tol) & (s+t<1-tol)))
    //      (s>tol) & (t>tol) & (s+t<1-tol)    
    //    }
    pit = 0;
    for (j=0; j<vertices.nrow(); j++) {
      if ((j!=t1) && (j!=t2) && (j!=t3)) {
        px = vertices(j,0); py = vertices(j,1);
        a = (-p1y*p2x + p0y*(-p1x + p2x) + p0x*(p1y - p2y) + p1x*p2y);
        s = ((p0y*p2x - p0x*p2y + (p2y - p0y)*px + (p0x - p2x)*py))/a;
        t = ((p0x*p1y - p0y*p1x + (p0y - p1y)*px + (p1x - p0x)*py))/a;
        if ((s>0) && (t>0) && (s+t<1)) pit++;
      }
    }
    ret[i] = pit;
  }
  return(ret);
}

//' Compute the number line intersections for a given web
//' 
//' @param g a web
//'  
//' @export
//' @return a vector with number of intersections for each line segment (Ingress: link)
//' 
//' @references \url{https://stackoverflow.com/questions/20519431/finding-point-of-intersection-in-r}
// [[Rcpp::export]]
List evaluateC (List g) {
  NumericMatrix vertices=g["vertices"], edges=g["edges"], faces=g["faces"];
  NumericVector map=g["map"], pts=g["pts"], errPit, errInter(edges.nrow()); 
  int i, j, n, max, l11, l12, l21, l22;
  double x1, x2, x3, x4, y1, y2, y3, y4, denom, ua, ub, tol=1e-10;
  // intersections
  max = 0;
  for (i=0; i<map.length(); i++) {
    if(!R_IsNA(map[i])) {
      if (map[i]>max) max = map[i];
    }   
  }
  NumericMatrix mapped(max, 2);
  for (i=0; i<map.length(); i++) {
    if (!R_IsNA(map[i])) {
      mapped(map[i]-1,0) = vertices(i,0);
      mapped(map[i]-1,1) = vertices(i,1);
    }   
  }
  //
  n = edges.nrow();
  for (i=0; i<(n-1); i++) {
    l11 = edges(i,0)-1; l12 = edges(i,1)-1;
    x1  = mapped(l11,0); y1 = mapped(l11, 1);
    x2  = mapped(l12,0); y2 = mapped(l12, 1);    
    for (j=i+1; j<n; j++) {
  //    ssi <- function(x1, x2, x3, x4, y1, y2, y3, y4, tol=1e-10){
  //# https://stackoverflow.com/questions/20519431/finding-point-of-intersection-in-r
  //      denom <- ((y4 - y3)*(x2 - x1) - (x4 - x3)*(y2 - y1))
  //      if (abs(denom) < tol) return(FALSE)
  //        ua <- ((x4 - x3)*(y1 - y3) - (y4 - y3)*(x1 - x3)) / denom
  //        ub <- ((x2 - x1)*(y1 - y3) - (y2 - y1)*(x1 - x3)) / denom
  //        return((ua > 0) & (ua < 1) & (ub > 0) & (ub < 1))
  //    }
      l21 = edges(j,0)-1; l22 = edges(j,1)-1;
      if ((l11!=l21) & (l11!=l22) & (l12!=l21) & (l12!=l22)) {
        x3 = mapped(l21,0); y3 = mapped(l21, 1);
        x4 = mapped(l22,0); y4 = mapped(l22, 1);   
        //
        denom = (y4 - y3)*(x2 - x1) - (x4 - x3)*(y2 - y1);
        if (std::abs(denom) >= tol) {
          ua = ((x4 - x3)*(y1 - y3) - (y4 - y3)*(x1 - x3)) / denom;
          ub = ((x2 - x1)*(y1 - y3) - (y2 - y1)*(x1 - x3)) / denom;
          if ((ua>0) & (ua<1) & (ub>0) & (ub<1)) {
            errInter[i]++; errInter[j]++;
          }
        }   
      }
    }
  }
  //
  errPit = pointsInTrianglesC(mapped, faces);
  for (i=0; i<faces.nrow(); i++) { 
    if (errPit[i]<pts[i]) errPit[i] = (pts[i]-errPit[i]); else errPit[i]=0;
  }
  return(List::create(Named("Intersection")=errInter, Named("VertDiff")=errPit));
}