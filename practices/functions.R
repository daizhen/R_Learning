test.any <-function()
{
	x<-1:10
	if(any(x >8))
	{
		print("Yes")
	}
	else
	{
		print("No")
	}
	print(x)
}

wrtlst <- function(lst)
{
	frm <- data.frame()
	row <- 1
	for(key in names(lst))
	{
		frm[row,1] <- key;
		frm[row,2] <- lst[key]
		row <-row+1
	}
	return(frm);
}

g <-1

change_g <- function()
{
	g <- g+1
	print(g);
	g<<-g
	print(g);
}
