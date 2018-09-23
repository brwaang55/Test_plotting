library(rgl)
library(Rvcg)
library(FNN)

#Function for computing the barycenter/centroid of a face.
compute_face_centroid=function(vertices,face){
  vertex=vertices[,face][-4,]
  centroid=apply(X=vertex,MARGIN = 1,FUN = mean)
  return(centroid)
}
#Function for Returning the barycenters of all the faces
find_face_coordinates=function(complex){
  coords=apply(X=complex$it,MARGIN = 2,FUN = compute_face_centroid,vertices=complex$vb)
  return(t(coords))
}
#Function for finding the k nearest faces to a given point. Can be done via the k nearest neighbor algorithm, or just considering the neihboring faces of the first closest face
find_closest_faces=function(complex,point,face_coords,n,nearest_neighbor=FALSE){
  face_list=c()
  if (nearest_neighbor==TRUE){
    point_index=knnx.index(data = face_coords,query = point,k = n)
    return(point_index)
  }
  else{
    point_index=knnx.index(data = face_coords,query = point,k = 1)
    base=point_index[1]
    for (i in 1:ceil(n/2)){
      point_index=c(point_index,base+i)
      point_index=c(point_index,base-i)
    }
    #point=matrix(face_coords[point_index,],ncol=3)
    face_list=c(face_list,point_index)
    return(face_list)
  }
}
#Read in file
file=vcgImport('m1810.off')
#Initialize Colors
colors=rep('white',dim(file$it)[2]*3)
#Find face barycenters of this complex
face_coords=find_face_coordinates(file)
#Find the closest faces of the complex

closest_faces=find_closest_faces(complex=file,point=matrix(file$vb[-4,2],ncol = 3),face_coords = face_coords,n =10,nearest_neighbor = TRUE)
#Coloring the '4' indices of the faces red
vert2=c()
for (i in closest_faces){
  base=i*3+1
  vert2=c(vert2,base,base+1,base+2,base+3)
}
#Setting the color to be red
colors[vert2]='red'
#Setting the material
file$material=list(color=colors)
plot3d(file)
rgl.points(matrix(file$vb[-4,2],ncol = 3),size=10,color='blue')

