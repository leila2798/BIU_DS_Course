use BoxOffice

--1. Get the top ten popular movies where the original language was English.
SELECT TOP (10) --*
	movie_id, original_title, popularity, original_language, release_date, runtime
  FROM dbo.movies
  WHERE original_language = 'en'
  ORDER BY popularity DESC

--2.Calculate the number of movies that were released by year.
SELECT YEAR(release_date) AS R_Year, count(movie_id) AS Movies_cnt
FROM dbo.movies
GROUP BY YEAR(release_date)
ORDER BY R_Year

--3.Calculate the number of movies that were released by month.
SELECT MONTH(release_date) AS R_Month, COUNT(movie_id) AS Movies_cnt
FROM dbo.movies
GROUP BY MONTH(release_date)
ORDER BY R_Month

/*
	4. Create a new variable based on runtime, where the movies are categorized into the following categories: 
		0 = Unknown, 1-50 = Short, 51-120 Average, >120 Long.
*/
SELECT movie_id, original_title, runtime,
	CASE WHEN (runtime >= 1) and (runtime<51) THEN ('Short')
	     WHEN (runtime>=51) and (runtime<=120) THEN ('Average')
		 WHEN (runtime>120) THEN ('Long')
		 WHEN (runtime<1) THEN ('Unknown')
		 ELSE ('N/A')
	END AS DurationCategory
FROM dbo.movies
ORDER BY runtime ASC

/*
	5. For each year, calculate :
		a. The dense rank, based on the revenue (descending order)
		b. The yearly revenue's sum of the movies
		c. The percent of the revenue with respect to the yearly annual revenue (b).
*/

SELECT tblMov.movie_id, tblMov.original_title, YEAR (tblMov.release_date) as release_year, tblMov.revenue, 
	DENSE_RANK() over (partition by YEAR (tblMov.release_date) order by revenue DESC) as drank_revenue, 
	tblRevenueSum.YearlyRevenue, (tblMov.revenue / tblRevenueSum.YearlyRevenue)*100 as RevenuePercent	
from dbo.movies as tblMov
LEFT OUTER JOIN
(SELECT  YEAR (release_date) as ReleaseYear, SUM (CAST(revenue as float)) as YearlyRevenue
from dbo.movies 
group by YEAR (release_date)) as tblRevenueSum
on YEAR (tblMov.release_date) = tblRevenueSum.ReleaseYear

/*
	6. For each movie: 
		a. Count the number of female actors in the movie. 
		b. Count the number of male actors in the movie. 
		c. Calculate the ratio of male vs women (female count / male count) *
*/

--using views for the code to be readable; not to overcrowd the final query

--join movies, actors_dim, movies_cast into View, to have the info in a single view
CREATE OR ALTER VIEW dsuser13.Movies_Actors_V
	AS
	SELECT tbl_cast.actor_id, tbl_actors.name, tbl_actors.gender, 
	tbl_cast.movie_id, tbl_movies.original_title as movie_name
	FROM dbo.movies_cast AS tbl_cast
	LEFT OUTER JOIN dbo.actors_dim AS tbl_actors
	   ON tbl_cast.actor_id = tbl_actors.actor_id
	LEFT OUTER JOIN movies AS tbl_movies
	   ON tbl_cast.movie_id = tbl_movies.movie_id

--select * from dsuser13.Movies_Actors_V

--helping view, includes q A and B
CREATE OR ALTER VIEW dsuser13.Actors_Gender_V
AS
select movie_id, movie_name, count(distinct (CASE WHEN gender = '1' THEN actor_id END)) AS female_actors,
		count(distinct (CASE WHEN gender = '2' THEN actor_id END)) AS male_actors
		--,female_actors / NULLIF(male_actors,0) AS gender_ratio
	from dsuser13.Movies_Actors_V
	group by movie_id, movie_name

--final query
select *, CAST(female_actors as float) / NULLIF(male_actors,0) AS gender_ratio
from dsuser13.Actors_Gender_V
order by movie_id

/*
	7. For each of the following languages: [en, fr, es, de, ru, it, ja]: 
	Create a column and set it to 1 if the movie has a translation** to the language and zero if not. 
*/

select lang.movie_id, movies.original_title as title,
	--case-when for diff languages
	CASE WHEN lang.movie_id = Any(select movie_id
		from movie_languages where iso_639_1 = 'en') THEN '1'ELSE '0' END AS en ,
	CASE WHEN lang.movie_id = Any(select movie_id
		from movie_languages where iso_639_1 = 'fr')THEN '1'ELSE '0' END AS fr,
	CASE WHEN lang.movie_id = Any(select movie_id
		from movie_languages where iso_639_1 = 'es') THEN '1'ELSE '0' END AS es,
	CASE WHEN lang.movie_id = Any(select movie_id
		from movie_languages where iso_639_1 = 'de') THEN '1'ELSE '0' END AS de,
	CASE WHEN lang.movie_id = Any(select movie_id
		from movie_languages where iso_639_1 = 'ru') THEN '1'ELSE '0' END AS ru,
	CASE WHEN lang.movie_id = Any(select movie_id
		from movie_languages where iso_639_1 = 'it') THEN '1'ELSE '0' END AS it,
	CASE WHEN lang.movie_id = Any(select movie_id
		from movie_languages where iso_639_1 = 'ja') THEN '1'ELSE '0' END AS ja
from movie_languages as lang
LEFT OUTER JOIN movies
on lang.movie_id = movies.movie_id
group by lang.movie_id, movies.original_title
order by lang.movie_id

/*
	8. For each of the crew departments, get a column and count the total number of individuals for each movie. 
	Create a view with this query
*/

--get the dpt names 
select distinct department
from dbo.movies_crew
order by department ASC

/* result: dpt
Actors
Art
Camera
Costume & Make-Up
Crew
Directing
Editing
Lighting
Production
Sound
Visual Effects
Writing
*/

-- Final View
CREATE OR ALTER VIEW dsuser13.Crew_By_Dpt_V
AS
select tbl.movie_id,
	(select count(*) from movies_crew as tbl1 where tbl1.department = 'Actors' and tbl1.movie_id = tbl.movie_id) as Actors,
	(select count(*) from movies_crew as tbl1 where tbl1.department = 'Art' and tbl1.movie_id = tbl.movie_id) as Art,
	(select count(*) from movies_crew as tbl1 where tbl1.department = 'Camera' and tbl1.movie_id = tbl.movie_id) as Camera,
	(select count(*) from movies_crew as tbl1 where tbl1.department = 'Costume & Make-Up' and tbl1.movie_id = tbl.movie_id) as Costume_MakeUp,
	(select count(*) from movies_crew as tbl1 where tbl1.department = 'Crew' and tbl1.movie_id = tbl.movie_id) as Crew,
	(select count(*) from movies_crew as tbl1 where tbl1.department = 'Directing' and tbl1.movie_id = tbl.movie_id) as Directing,
	(select count(*) from movies_crew as tbl1 where tbl1.department = 'Editing' and tbl1.movie_id = tbl.movie_id) as Editing,
	(select count(*) from movies_crew as tbl1 where tbl1.department = 'Lighting' and tbl1.movie_id = tbl.movie_id) as Lighting,
	(select count(*) from movies_crew as tbl1 where tbl1.department = 'Production' and tbl1.movie_id = tbl.movie_id) as Production,
	(select count(*) from movies_crew as tbl1 where tbl1.department = 'Sound' and tbl1.movie_id = tbl.movie_id) as Sound,
	(select count(*) from movies_crew as tbl1 where tbl1.department = 'Visual Effects' and tbl1.movie_id = tbl.movie_id) as Visual_Effects,
	(select count(*) from movies_crew as tbl1 where tbl1.department = 'Writing' and tbl1.movie_id = tbl.movie_id) as Writing
	from dbo.movies_crew as tbl
group by movie_id
