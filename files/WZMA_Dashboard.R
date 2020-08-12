func_run_two_page_report = function(wz, 
                                    odbChannel, 
                                    OUTPUT_DIRECTORY, 
                                    table_name_wz, 
                                    table_name_prior,
                                    threshold_speed_congestion, 
                                    threshold_speed_queue,
                                    segments_shp, 
                                    counties_shp,
                                    path_ppt_file) {
    
    
    
    wz_color = 'darkorange'
    prior_color = 'gray49'
    
    # If heatmap_hourly = TRUE the heatmap will be drawn with hourly bins. If set to to FALSE, the bins span 5 mins.
    heatmap_hourly = FALSE
    
    
    wz$wz_id = as.character(wz$wz_id)
    
    wz = wz %>% dplyr::select(wz_id, county, roadway, category, direction, speed, 
                              start_datetime, end_datetime, start_mm, end_mm, tmc_list)
    
    
    setnames(wz,
             old = c('tmc_list', 'start_datetime', 'end_datetime', 'roadway'),
             new = c('tmc', 'wz_start', 'wz_end', 'road'))
    
    wz$tmc_count = sapply(str_split(wz$tmc, ','), length)
    wz = drop_na(wz)
    
    func_get_dates = function(wz) {
        wz$wz_start = as.POSIXct(wz$wz_start, format = "%m/%d/%Y %H:%M", tz='') 
        wz$wz_end = as.POSIXct(wz$wz_end, format = "%m/%d/%Y %H:%M", tz='')
        
        wz$wz_prior_start = wz$wz_start - lubridate::duration(num=1, units='year')
        wz$wz_prior_end = wz$wz_end - lubridate::duration(num=1, units='year')
        
        wz$wz_year_start = lubridate::make_datetime(year=lubridate::year(wz$wz_start), month = 1L, day = 1L, hour = 0L, min = 0L, sec = 0)
        wz$wz_year_end = lubridate::make_datetime(year=lubridate::year(wz$wz_end), month = 12L, day = 31L, hour = 23L, min = 59L, sec = 59) 
        
        wz$wz_prior_year_start = lubridate::make_datetime(year=lubridate::year(wz$wz_prior_start), month = 1L, day = 1L, hour = 0L, min = 0L, sec = 0)
        wz$wz_prior_year_end = lubridate::make_datetime(year=lubridate::year(wz$wz_prior_end), month = 12L, day = 31L, hour = 23L, min = 59L, sec = 59) 
        
        return(wz)
    }
    
    wz = func_get_dates(wz)
    
    # wz = wz %>% filter(wz_start >= wz_date_greater_than, 
    #                    wz_end < wz_date_smaller_than, 
    #                    wz_start < wz_end)
    
    
    # The workzone should be at least an hour long
    wz$wz_duration = difftime(wz$wz_end, wz$wz_start, units = 'hour') %>% as.numeric()
    wz = wz %>% filter(wz_duration > 1)
    
    
    
    
    row.names(wz) = NULL
    ################################################################
    
    output_dir = paste0(OUTPUT_DIRECTORY, "Reports")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)    
    }
    
    for (i in seq_len(nrow(wz))) {
        output_dir = paste0(OUTPUT_DIRECTORY, wz$wz_id[i])
        if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)    
        }
    }
    
    
    # ###############################
    # na.pad <- function(x,len){
    #     x[1:len]
    # }
    # makePaddedDataFrame <- function(l,...){
    #     maxlen <- max(sapply(l,length))
    #     data.frame(lapply(l,na.pad,len=maxlen),...)
    # }
    ######################################################################### read tmc attribute table
    
    
    
    func_create_tmc_order = function() {
        query = "select * from [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen]"
        mile_marker = sqlQuery(odbChannel, query)
        # odbcClose(odbChannel)
        
        setnames(mile_marker, old=c('Start_MM', 'End_MM'), new=c('start_mm', 'end_mm'))
        mile_marker = mile_marker %>% select (tmc, start_mm, end_mm)
        
        
        df_list = list()
        
        for (i in seq_len(nrow(wz))) {
            df_list[[i]] = data.frame(wz_id = wz$wz_id[i], tmc = str_split(wz$tmc[i], ',')[[1]] %>% unique() ) # %>% print()
        }
        df_tmc_order=rbindlist(df_list)
        
        df_tmc_order = merge(x=df_tmc_order, y=mile_marker, by.x='tmc', by.y='tmc')
        df_tmc_order = df_tmc_order[, .(tmc, start_mm, end_mm, tmc_order = rank(start_mm, ties.method = 'first')), by = wz_id]
        df_tmc_order = df_tmc_order %>% arrange(wz_id, tmc_order) %>% as.data.table()
        df_tmc_order$tmc = as.character(df_tmc_order$tmc)
        
        return (df_tmc_order)
    }
    
    df_tmc_order = func_create_tmc_order()
    
    #######################
    func_get_tmc_string = function(wz_tmc) {
        
        wz_tmc = str_split(wz_tmc, ',')[[1]]
        tmc_string = ''
        for (tmc in wz_tmc) {
            tmc_string = paste0("'", tmc,"',", tmc_string)
        }
        tmc_string = str_sub(tmc_string, end=-2)
        return(tmc_string)
        
    }
    

    ####################################################### Create Geo Map of workzone

    func_create_map = function(wz_id, wz_tmc, OUTPUT_DIRECTORY, segments_shp, counties_shp) {
        
        # wz_tmc = wz$tmc[i]
        tmc = str_split(wz_tmc, ',')[[1]]
        # wz_id = wz$wz_id[i]
        
        # st_geometry_type(df) %>% head()
        # st_crs(df)
        # st_bbox(df)
        # df
        # tmc = c('108P04226','108+04227','108P04227','108+04228','108P04228','108+04229','108P04229','108+04230')
        df2 = segments_shp[segments_shp$TMC %in% tmc, ] 
        # lat = df2$StartLat %>% mean()
        # lon = df2$StartLong %>% mean()
        
        
       
        
        # At 45 degrees north or south, the distance between longitudes is about 49 miles (79 kilometers).
        # The distance between latitudes is always 69 miles or 110 km.
        69/49
        
        if (nrow(df2) > 0) {
            bbox = c(mean(c(df2$StartLong, df2$EndLong)) %>% min(), 
                     mean(c(df2$StartLat, df2$EndLat)) %>% min(), 
                     mean(c(df2$StartLong, df2$EndLong)) %>% max(), 
                     mean(c(df2$StartLat, df2$EndLat)) %>% max())
            # bbox[1] = bbox[1] - 0.14  # left
            # bbox[2] = bbox[2] - 0.1   # bottom
            # bbox[3] = bbox[3] + 0.14  # right
            # bbox[4] = bbox[4] + 0.1   # top
            
            bbox[1] = bbox[1] - 0.12  # left
            bbox[2] = bbox[2] - 0.12 # bottom
            bbox[3] = bbox[3] + 0.12  # right
            bbox[4] = bbox[4] + 0.12  # top
            
            map_tmc = ggmap::get_stamenmap(bbox = bbox, maptype = 'toner-lite', zoom = 11)
            
            map_tmc = ggmap(map_tmc)
            
            
            wz_zoomed_in = map_tmc + 
                geom_sf(data = df2, size = 2, alpha=0.1, color = "orange", fill = "cyan1", inherit.aes = FALSE) + 
                ggtitle("") + 
                # scale_x_continuous(breaks=seq(bbox[1], bbox[3], length.out=5), labels=seq(bbox[1], bbox[3], length.out=5) %>% round(2), expand=expand_scale()) + 
                coord_sf() + 
                theme_bw() +
                theme(axis.title = element_blank(),
                      # axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks = element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      plot.margin = unit(c(0,0,0,0), "cm")) # ("left", "right", "bottom", "top")
            
            
        }else {
            bbox = c()
            wz_zoomed_in = ggplot()
        }
        
        
  
        
        case_map = ggplot()
        case_map = case_map +  
            geom_sf(data = counties_shp, size = 0.3, color = "gray80", fill = "lightcyan", alpha=1) + 
            coord_sf()
        
        bbox
        
        case_map_with_wz = case_map + 
            geom_sf(data = df2, size = 1.4, color = "orange", fill = "cyan1") + 
            ggtitle("") + 
            coord_sf() + 
            theme_bw() + 
            theme(# axis.title = element_blank(),
                # axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                plot.margin = unit(c(0,0,0,0), "cm"))# ("left", "right", "bottom", "top")
        
        
       
        main.plot = wz_zoomed_in
        inset.plot = case_map_with_wz
        plot.with.inset <-
            ggdraw() +
            draw_plot(main.plot, x=0, y=0, width=1, height=1) +
            draw_plot(inset.plot, x = 0.7, y = 0.57, width = 0.4, height = 0.4)
        
        ggsave(plot=plot.with.inset, paste0(OUTPUT_DIRECTORY, wz_id, "/", wz_id,"_geo_map", ".png"), dpi=300, width=6, height=4)
    }
    
    
    ############## SQL heatmap #################
    func_sql_heatmap = function(table_name, tmc_string, wz_id_value, wz_start, wz_end, df_tmc_order) {
        
        query = paste0("SELECT 
                   tmc_code as tmc,
                   DATEADD(MINUTE, DATEDIFF(MINUTE, 0, time_stamp1)/5*5, 0) AS probe_5_minutes,
                   CAST(CONCAT(AVG(DATEPART(YEAR, [time_stamp1])),'-',
                   AVG(DATEPART(MONTH, [time_stamp1])),'-',
                   AVG(DATEPART(DAY, [time_stamp1])),' ',
                   AVG(DATEPART(HOUR, [time_stamp1])),':',
                   AVG(DATEPART(MINUTE, [time_stamp1])),':00') AS DATETIME) AS min_Probe,
                   
                   CAST(CONCAT(AVG(DATEPART(YEAR, [time_stamp1])),'-',
                   AVG(DATEPART(MONTH, [time_stamp1])),'-',
                   AVG(DATEPART(DAY, [time_stamp1])),' ',
                   AVG(DATEPART(HOUR, [time_stamp1])),':00') AS DATETIME) AS hr_Probe,
                   AVG([speed]) AS Avgspeed_PROBE,
                   AVG([calculated_travel_time]) AS TT_PROBE,
                   COUNT ([speed]) as Count_Probe
                   FROM (
                   SELECT *
                   FROM ", table_name ,"
                   WHERE 
                   tmc_code in (", tmc_string ,") AND
                   [calculated_travel_time] > '0' AND
                   [calculated_travel_time] IS NOT NULL AND
                   [time_stamp1] > '", wz_start, "' AND
                   [time_stamp1] < '", wz_end, "' 
                   ) AS first_table 
                   GROUP BY
                   tmc_code,
                   DATEADD(MINUTE, DATEDIFF(MINUTE, 0, time_stamp1)/5*5, 0)")
        
        wz_sql_heatmap = sqlQuery(odbChannel, query)
        
        x = wz_sql_heatmap %>% as.data.table()
        
        # x$min5 = lubridate::round_date(d$probe_timestamp, unit="5 min") # %>% lubridate::minute()
        # head(d)
        
        minute_data = x[, .(min5 = seq(from = wz_start, to = wz_end, by = "5 min", tz='')), by=.(tmc)]
        minute_data$hr_Probe = floor_date(minute_data$min5, unit="1 hour")
        
        x = full_join(x, minute_data, by=c('tmc', 'hr_Probe','probe_5_minutes'='min5')) %>% as.data.table()
        x <- merge(x, df_tmc_order %>% filter(wz_id==wz_id_value), by.x = "tmc", by.y = "tmc")
        x = x[order(probe_5_minutes, tmc_order), ]
        
        return (x)
    }
    
    # wz_sql_heatmap = func_sql_heatmap(table_name_wz, tmc_string, wz_id_value, wz_start, wz_end, df_tmc_order)
    
    ### Heatmap plot ###
    func_create_heatmap_plot = function(wz_id, wz_tmc, wz_start, wz_end, tmc_count, wz_Start_MM, wz_End_MM, wz_sql_heatmap) {
        
        
        x = wz_sql_heatmap %>% as.data.table()
        
        
        if (nrow(x) > 0) {
            # setnames(x, 'tmc_code', 'tmc')
            x$tmc = as.character(x$tmc)
            
            if (heatmap_hourly == TRUE) {
                
                x$hour = hour(x$hr_Probe)
                x$day = day(x$hr_Probe)
                x$month = month(x$hr_Probe)
                x$year = year(x$hr_Probe)
                
                x = x %>% arrange(tmc_order, year, month, day, hour) %>% as.data.table()
                # ===========================================================
                x2 = x[, .(Avgspeed_PROBE = mean(Avgspeed_PROBE)), # TT_PROBE = mean(TT_PROBE)),
                       by = .(wz_id, start_mm, end_mm, tmc, tmc_order, hr_Probe,  year, month, day, hour)]
                
            } else {
                
                x$min = minute(x$probe_5_minutes)
                x$hour = hour(x$probe_5_minutes)
                x$day = day(x$probe_5_minutes)
                x$month = month(x$probe_5_minutes)
                x$year = year(x$probe_5_minutes)
                
                x = x %>% arrange(tmc_order, year, month, day, hour, min) %>% as.data.table()
                # ===========================================================
                x2 = x[, .(Avgspeed_PROBE = mean(Avgspeed_PROBE)), # TT_PROBE = mean(TT_PROBE)), 
                       by = .(wz_id, start_mm, end_mm, tmc, tmc_order, hr_Probe,  year, month, day, hour, min)]
                
                
            }
            ###########
            
            
            
            
            
            #########
            x2$x_coor = 1
            
            x2$x_left = x2[, .(cumsum_x_coor = cumsum(x_coor)), by = .(tmc_order)][,cumsum_x_coor]
            x2$x_left = x2$x_left - 1
            
            x2$x_right = x2$x_left + 1
            
            
            y_coor = x2[, .(y_bottom = head(start_mm,1)), keyby = .(tmc_order, tmc)]
            y_coor$y_top = shift(y_coor$y_bottom, 1, type = 'lead')
            
            tmc_end_last_segment = x2$end_mm %>% tail(1)
            
            # y_coor$y_top[tail(y_coor$tmc_order, 1)] = tmc_end_last_segment
            
            y_coor[nrow(y_coor), 'y_top'] = tmc_end_last_segment
            
            # tail(y_coor$y_top, 2) %>% head(1) + tmc_len_last_segment 
            y_coor
            x2 = merge(x2, y_coor, by = c('tmc_order', 'tmc'))
            
            
            # x2$x_left = x2$hr_Probe
            head(x2$hr_Probe)
            
            time_points_x_axis = x2[hour == 0, .(hr_Probe = head(hr_Probe, 1), 
                                                 x_left = head(x_left, 1)), 
                                    keyby = .(year, month, day)]
            
            
            time_points_x_axis$labels = date_format("%m/%d\n%a", tz='')(time_points_x_axis$hr_Probe)
            
            
            # Select very third row in the x axis labels and break points.
            wz_duration = difftime(wz_end, wz_start, units = 'day') %>% as.numeric()
            if (wz_duration <= 1) {
                wz_duration_category = 'very_short' # Every hour?
                # time_points_x_axis_filtered = time_points_x_axis[seq(1, nrow(time_points_x_axis), 1), ]
                # time_points_x_axis_filtered = x2$hour %>% unique() %>% sort()
                time_points_x_axis = x2[min == 0, .(hr_Probe = head(hr_Probe, 1), 
                                                    x_left = head(x_left, 1)), 
                                        keyby = .(hour, min)]
                
                
                time_points_x_axis$labels = date_format("%H:%M\n%a", tz='')(time_points_x_axis$hr_Probe)
                if (nrow(time_points_x_axis) <= 10) { # 10 hours work zone
                    time_points_x_axis_filtered = time_points_x_axis[seq(1, nrow(time_points_x_axis), 1), ]
                } else {
                    time_points_x_axis_filtered = time_points_x_axis[seq(1, nrow(time_points_x_axis), 2), ]
                }
                
                
            } else if (wz_duration < 8) { 
                wz_duration_category = 'short' # Every day
                time_points_x_axis_filtered = time_points_x_axis[seq(1, nrow(time_points_x_axis), 1), ]
            } else if (wz_duration < 22) { 
                wz_duration_category = 'medium' # Every 3 days
                time_points_x_axis_filtered = time_points_x_axis[seq(1, nrow(time_points_x_axis), 3), ]
            } else if (wz_duration < 50) { 
                wz_duration_category = 'long' # Every 7 days
                time_points_x_axis_filtered = time_points_x_axis[seq(1, nrow(time_points_x_axis), 7), ]
            } else {
                wz_duration_category = 'very_long' # Every 30 days
                time_points_x_axis_filtered = time_points_x_axis[seq(1, nrow(time_points_x_axis), 30), ]
            }
            
            # time_points_x_axis_filtered = time_points_x_axis[seq(1, nrow(time_points_x_axis), 1), ]
            
            
            # time_points_x_axis = left_join(time_points_x_axis[, .(hour, min, hr_Probe, x_left)], 
            #                                time_points_x_axis_filtered, by = c('hour','min','hr_Probe','x_left'))
            
            if (wz_duration > 1) {
                time_points_x_axis$labels = ""
                time_points_x_axis = left_join(time_points_x_axis[, .(year,month,day,hr_Probe,x_left)], 
                                               time_points_x_axis_filtered, by = c('year','month','day','hr_Probe','x_left'))
            } else {
                time_points_x_axis$labels = ""
                time_points_x_axis = left_join(time_points_x_axis[, .(hour, min, hr_Probe, x_left)],
                                               time_points_x_axis_filtered, by = c('hour','min','hr_Probe','x_left'))
            }
            
            time_points_x_axis[is.na(time_points_x_axis)] = ""
            
            # x2$x_right = shift(x2$hr_Probe,1,type='lead')
            # head(shift(x2$hr_Probe,1,type='lead'))
            
            # print(str(x2))
            
            y_coordinates = c(y_coor$y_bottom, tail(y_coor$y_top,1))
            
            y_coordinates2 = ceiling(y_coordinates)
            
            y_coordinates2 = seq(from=head(y_coordinates2,1), to=tail(y_coordinates2,1), by=1) 
            
            
            
            g = ggplot(data=x2) +
                geom_rect(aes(xmin=x2$x_left, xmax=x2$x_right, ymin=x2$y_bottom, ymax=x2$y_top, fill=Avgspeed_PROBE)) +
                geom_hline(yintercept = c(wz_Start_MM, wz_End_MM), size=0.5) + 
                scale_fill_distiller(name = 'speed', type= 'div', palette = 'RdYlGn', direction=1, na.value='white',
                                     limits = c(0,80)) +
                scale_y_continuous(name = 'Mile Marker', expand = expand_scale(), breaks = y_coordinates2, 
                                   labels=y_coordinates2 %>% round(1)) +
                # scale_x_continuous(expand = expand_scale(), breaks = time_points_x_axis$x_left, 
                #                    labels = date_format("%m/%d\n%a")(time_points_x_axis$hr_Probe)) +
                scale_x_continuous(expand = expand_scale(), 
                                   breaks = time_points_x_axis$x_left, 
                                   labels = time_points_x_axis$labels) +
                
                
                # scale_x_datetime(expand = expand_scale(), 
                #                  date_breaks = "3 days" , 
                #                  labels = date_format("%m/%d\n%a"),
                #                  date_minor_breaks = "1 day",
                #                  limits = c(x2$hr_Probe%>%min(), x2$hr_Probe%>%max())) +
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      axis.text.x = element_text(colour="black", angle=0, hjust=0.5, size = 18),
                      axis.text.y = element_text(colour="black", size = 14),
                      # axis.ticks.x = element_line(),
                      axis.title.y = element_blank(),
                      legend.direction = "horizontal",
                      legend.position = "none") 
        } else {
            g = ggplot()
        }
        
        
        
        ggsave(paste0(OUTPUT_DIRECTORY, wz_id, "/", wz_id,"_TT_heatmap_speed", ".png"), dpi=300, width=14, height=4)
        
    }
    
    
    
    
    # func_create_heatmap_plot(wz_id, wz_tmc, wz_start, wz_end, tmc_count, wz$start_mm[i], wz$end_mm[i], wz_sql_heatmap)
    
    ### SQL Scatter ###
    func_sql_scatter = function (table_name, tmc_string, wz_start, wz_end, tmc_count) {
        
        
        query = paste0("SELECT
                   min_Probe_5 AS probe_timestamp,
                   DATENamE(dw, min_Probe_5) AS day_of_week,
                   DATEPART(HOUR, min_Probe_5) AS probe_hour,
                   ROUND(DATEPART(MINUTE,min_Probe_5)/5,0)*5 AS probe_minute,
                   (case when month(min_Probe_5) in (12, 1, 2) then 'winter'
                   when month(min_Probe_5) in (3, 4, 5) then 'spring'
                   when month(min_Probe_5) in (6, 7, 8) then 'summer'
                   when month(min_Probe_5) in (9, 10, 11) then 'autumn'
                   end) AS probe_season,
                   traveltime AS probe_tt
                   FROM(
                   
                   SELECT
                   CAST(CONCAT(AVG(DATEPART(YEAR, min_Probe)),'-',
                   AVG(DATEPART(MONTH, min_Probe)),'-',
                   AVG(DATEPART(DAY, min_Probe)),' ',
                   AVG(DATEPART(HOUR, min_Probe)),':',
                   AVG(DATEPART(MINUTE, min_Probe)),':00') AS DATETIME) AS min_Probe_5,
                   SUM(TT_PROBE) AS traveltime,
                   COUNT(tmc_code) AS tmc_count
                   FROM(
                   SELECT 
                   tmc_code,
                   CAST(CONCAT(AVG(DATEPART(YEAR, [time_stamp1])),'-',
                   AVG(DATEPART(MONTH, [time_stamp1])),'-',
                   AVG(DATEPART(DAY, [time_stamp1])),' ',
                   AVG(DATEPART(HOUR, [time_stamp1])),':',
                   AVG(DATEPART(MINUTE, [time_stamp1])),':00') AS DATETIME) AS min_Probe,
                   
                   CAST(CONCAT(AVG(DATEPART(YEAR, [time_stamp1])),'-',
                   AVG(DATEPART(MONTH, [time_stamp1])),'-',
                   AVG(DATEPART(DAY, [time_stamp1])),' ',
                   AVG(DATEPART(HOUR, [time_stamp1])),':00') AS DATETIME) AS hr_Probe,
                   AVG([speed]) AS Avgspeed_PROBE,
                   AVG([calculated_travel_time]) AS TT_PROBE,
                   COUNT ([tmc_code]) as Count_Probe
                   FROM (
                   SELECT *
                   FROM ", table_name ,"
                   WHERE 
                   tmc_code in (", tmc_string ,") AND
                   [calculated_travel_time] > '0' AND
                   [calculated_travel_time] IS NOT NULL AND
                   [time_stamp1] > '", wz_start, "' AND
                   [time_stamp1] < '", wz_end, "' 
                   ) AS first_table 
                   GROUP BY
                   DATEPART(YEAR, [time_stamp1]),
                   DATEPART(MONTH, [time_stamp1]),
                   DATEPART(DAY, [time_stamp1]),
                   DATEPART(HOUR, [time_stamp1]),
                   (DATEPART(MINUTE, [time_stamp1])/5),
                   tmc_code
                   --ORDER BY min_Probe
                   ) AS NEW
                   GROUP BY
                   DATEPART(YEAR, min_Probe),
                   DATEPART(MONTH, min_Probe),
                   DATEPART(DAY, min_Probe),
                   DATEPART(HOUR, min_Probe),
                   (DATEPART(MINUTE, min_Probe)/5)
                   ) AS third_table
                   WHERE 
                   tmc_count = '", tmc_count,"'")
        
        wz_sql_scatter = sqlQuery(odbChannel, query)
        
        return(wz_sql_scatter)
        
    }
    ### SQL Distribution ###
    func_sql_distribution = function (table_name, tmc_string, wz_start, wz_end, tmc_count, wz_sql_scatter) {
        
        query = paste0("SELECT DISTINCT
                   day_of_week,
                   probe_hour,
                   probe_minute,
                   probe_season,
                   PERCENTILE_DISC ( 0.05 ) WITHIN GROUP ( ORDER BY probe_tt ASC) OVER (PARTITION BY day_of_week,probe_hour,probe_minute,probe_season) AS travel_time_5th,
                   PERCENTILE_DISC ( 0.15 ) WITHIN GROUP ( ORDER BY probe_tt ASC) OVER (PARTITION BY day_of_week,probe_hour,probe_minute,probe_season) AS travel_time_15th,
                   PERCENTILE_DISC ( 0.25 ) WITHIN GROUP ( ORDER BY probe_tt ASC) OVER (PARTITION BY day_of_week,probe_hour,probe_minute,probe_season) AS travel_time_25th,
                   PERCENTILE_DISC ( 0.50 ) WITHIN GROUP ( ORDER BY probe_tt ASC) OVER (PARTITION BY day_of_week,probe_hour,probe_minute,probe_season) AS travel_time_50th,
                   PERCENTILE_DISC ( 0.75 ) WITHIN GROUP ( ORDER BY probe_tt ASC) OVER (PARTITION BY day_of_week,probe_hour,probe_minute,probe_season) AS travel_time_75th,
                   PERCENTILE_DISC ( 0.85 ) WITHIN GROUP ( ORDER BY probe_tt ASC) OVER (PARTITION BY day_of_week,probe_hour,probe_minute,probe_season) AS travel_time_85th,
                   PERCENTILE_DISC ( 0.95 ) WITHIN GROUP ( ORDER BY probe_tt ASC) OVER (PARTITION BY day_of_week,probe_hour,probe_minute,probe_season) AS travel_time_95th
                   FROM(SELECT
                   
                   DATENamE(dw, min_Probe_5) AS day_of_week,
                   DATEPART(HOUR, min_Probe_5) AS probe_hour,
                   ROUND(DATEPART(MINUTE,min_Probe_5)/5,0)*5 AS probe_minute,
                   (case when month(min_Probe_5) in (12, 1, 2) then 'winter'
                   when month(min_Probe_5) in (3, 4, 5) then 'spring'
                   when month(min_Probe_5) in (6, 7, 8) then 'summer'
                   when month(min_Probe_5) in (9, 10, 11) then 'autumn'
                   end) AS probe_season,
                   traveltime AS probe_tt
                   FROM(
                   
                   SELECT
                   CAST(CONCAT(AVG(DATEPART(YEAR, min_Probe)),'-',
                   AVG(DATEPART(MONTH, min_Probe)),'-',
                   AVG(DATEPART(DAY, min_Probe)),' ',
                   AVG(DATEPART(HOUR, min_Probe)),':',
                   AVG(DATEPART(MINUTE, min_Probe)),':00') AS DATETIME) AS min_Probe_5,
                   SUM(TT_PROBE) AS traveltime,
                   COUNT(tmc_code) AS tmc_count
                   FROM(
                   SELECT 
                   tmc_code,
                   CAST(CONCAT(AVG(DATEPART(YEAR, [time_stamp1])),'-',
                   AVG(DATEPART(MONTH, [time_stamp1])),'-',
                   AVG(DATEPART(DAY, [time_stamp1])),' ',
                   AVG(DATEPART(HOUR, [time_stamp1])),':',
                   AVG(DATEPART(MINUTE, [time_stamp1])),':00') AS DATETIME) AS min_Probe,
                   
                   CAST(CONCAT(AVG(DATEPART(YEAR, [time_stamp1])),'-',
                   AVG(DATEPART(MONTH, [time_stamp1])),'-',
                   AVG(DATEPART(DAY, [time_stamp1])),' ',
                   AVG(DATEPART(HOUR, [time_stamp1])),':00') AS DATETIME) AS hr_Probe,
                   AVG([speed]) AS Avgspeed_PROBE,
                   AVG([calculated_travel_time]) AS TT_PROBE,
                   COUNT ([tmc_code]) as Count_Probe
                   FROM (
                   SELECT *
                   FROM ", table_name ,"
                   WHERE 
                   tmc_code in (", tmc_string ,") AND
                   [calculated_travel_time] > '0' AND
                   [calculated_travel_time] IS NOT NULL AND
                   [time_stamp1] > '", wz_start, "' AND
                   [time_stamp1] < '", wz_end, "' 
                   ) AS first_table 
                   GROUP BY
                   DATEPART(YEAR, [time_stamp1]),
                   DATEPART(MONTH, [time_stamp1]),
                   DATEPART(DAY, [time_stamp1]),
                   DATEPART(HOUR, [time_stamp1]),
                   (DATEPART(MINUTE, [time_stamp1])/5),
                   tmc_code
                   --ORDER BY min_Probe
                   ) AS NEW
                   GROUP BY
                   DATEPART(YEAR, min_Probe),
                   DATEPART(MONTH, min_Probe),
                   DATEPART(DAY, min_Probe),
                   DATEPART(HOUR, min_Probe),
                   (DATEPART(MINUTE, min_Probe)/5)
                   ) AS third_table
                   WHERE 
                   tmc_count = '", tmc_count,"'
                   
                   --ORDER BY probe_timestamp 
                   ) AS second_table
                   ORDER BY
                   day_of_week,
                   probe_hour,
                   probe_minute,
                   probe_season
                   ")
        
        wz_sql_scatter_prior_distribution = sqlQuery(odbChannel, query)
        
        # wz_sql_distribution_prior = as.data.frame(merge(wz_sql_scatter_prior, wz_sql_scatter_prior_distribution, 
        #                                                 by= c("day_of_week", "probe_hour", "probe_minute", "probe_season")))
        
        
        # wz_sql_distribution = as.data.frame(merge(wz_sql_scatter, wz_sql_scatter_prior_distribution, 
        #                                           by= c("day_of_week", "probe_hour", "probe_minute", "probe_season")))
        
        return (wz_sql_scatter_prior_distribution)
        
    }
    
    ### New Travel Time scatter - distribution Plot ###
    func_create_distribution_scatter_plot = function(wz_id, wz_start, wz_end, 
                                                     wz_sql_distribution, OUTPUT_DIRECTORY) {
        
        if (nrow(wz_sql_distribution) < 1) {
            g=ggplot()
            ggsave(paste0(OUTPUT_DIRECTORY, "/", wz_id, "/", wz_id,"_TT_Scatter_distribution", ".png"), width = 7, height = 2)
            return()
        }
        
        d <- wz_sql_distribution %>% as.data.frame()
        
        
        
        
        d = d %>% arrange(probe_timestamp)
        d$min5 = lubridate::floor_date(d$probe_timestamp, unit="5 min") # %>% lubridate::minute()
        head(d)
        
        # date_hour = date_format("%m/%d/%Y %H:00", tz='')(d$probe_timestamp)
        # date_hour = as.POSIXct(date_hour, format = "%m/%d/%Y %H:%M", tz='')
        # d$date_hour = date_hour
        
        # To be able to find the missing  dates, I will join with the full array of hours from start_date to end_date
        minute_data = seq(from = wz_start, to = wz_end, by = "5 min", tz='')
        minute_data = data.frame("min5"=minute_data)
        d = full_join(d, minute_data, by='min5')
        # str(d)
        # df_ticks = d %>% group_by(year(probe_timestamp), month(probe_timestamp), day(probe_timestamp)) %>% summarise(n=n()) %>% cumsum()
        head(d)
        d_miss=d[is.na(d$day_of_week),]
        d_miss
        
        d_miss$probe_hour = lubridate::hour(d_miss$min5)
        d_miss$probe_minute = lubridate::minute(d_miss$min5)
        d_miss$day_of_week = lubridate::wday(d_miss$min5, label=TRUE, abbr=FALSE) %>% as.factor()
        d_miss$probe_timestamp = (d_miss$min5)
        head(d_miss)
        
        d2=d
        d2[is.na(d2$probe_tt),] = d_miss
        d_miss=d2[is.na(d2$probe_tt),]
        d_miss
        
        d=d2
        
        # start_date <- as.POSIXct(paste(date, ratio_plot_start_time, sep=" "))
        # end_date <- as.POSIXct(paste(date, ratio_plot_end_time, sep=" "))
        # 
        # insert_minor <- function(major_labs, n_minor) {labs <- 
        #   c( sapply( major_labs, function(x) c(x, rep("", 2) ) ) )
        # labs[1:(length(labs)-n_minor)]}
        
        
        # wz_duration = 10
        wz_duration = difftime(wz_end, wz_start, units = 'day') %>% as.numeric()
        if (wz_duration <= 1) {
            wz_duration_category = 'very_short' # Every hour?
            breaks_daily = seq(from = wz_start, to = wz_end, by = "1 hour")
            if (length(breaks_daily) <= 10) { # 10 hours of data
                labels_daily = format(seq(from = wz_start, to = wz_end, by = "1 hour"), "%H:%M\n%a")
                labs = c(sapply(labels_daily, function(x) {
                    c(x, rep("", 0))
                }))
                
            } else {
                labels_daily = format(seq(from = wz_start, to = wz_end, by = "2 hour"), "%H:%M\n%a")
                labs = c(sapply(labels_daily, function(x) {
                    c(x, rep("", 1))
                }))
            }
            
            labs = labs[1:(length(breaks_daily))]
            
        } else if (wz_duration < 8) { 
            wz_duration_category = 'short' # Every day
            breaks_daily = seq(from = trunc(wz_start,"days") + 60*60*24, to = wz_end, by = "1 day")
            labels_daily = format(seq(from = trunc(wz_start,"days") + 60*60*24, to = wz_end, by = "1 day"), "%m/%d\n%a")
            labs = c(sapply(labels_daily, function(x) {
                c(x, rep("", 0))
            }))
            
            labs = labs[1:(length(breaks_daily))]
            
        } else if (wz_duration < 22) { 
            wz_duration_category = 'medium' # Every 3 days
            breaks_daily = seq(from = trunc(wz_start,"days") + 60*60*24, to = wz_end, by = "1 day")
            labels_daily = format(seq(from = trunc(wz_start,"days") + 60*60*24, to = wz_end, by = "3 day"), "%m/%d\n%a")
            labs = c(sapply(labels_daily, function(x) {
                c(x, rep("", 2))
            }))
            
            labs = labs[1:(length(breaks_daily))]
            
        } else if (wz_duration < 50) { 
            wz_duration_category = 'long' # Every 7 days
            breaks_daily = seq(from = trunc(wz_start,"days") + 60*60*24, to = wz_end, by = "1 day")
            labels_daily = format(seq(from = trunc(wz_start,"days") + 60*60*24, to = wz_end, by = "7 day"), "%m/%d\n%a")
            labs = c(sapply(labels_daily, function(x) {
                c(x, rep("", 6))
            }))
            
            labs = labs[1:(length(breaks_daily))]
            
        } else {
            wz_duration_category = 'very_long' # Every 30 days
            breaks_daily = seq(from = trunc(wz_start,"days") + 60*60*24, to = wz_end, by = "1 day")
            labels_daily = format(seq(from = trunc(wz_start,"days") + 60*60*24, to = wz_end, by = "31 day"), "%m/%d\n%a")
            labs = c(sapply(labels_daily, function(x) {
                c(x, rep("", 30))
            }))
            
            labs = labs[1:(length(breaks_daily))]
            
        }
        
        # breaks_daily = seq(from = trunc(wz_start,"days") + 60*60*24, to = wz_end, by = "1 day")
        # labels_daily = format(seq(from = trunc(wz_start,"days") + 60*60*24, to = wz_end, by = "7 day"), "%m/%d\n%a")
        # 
        # labs = c(sapply(labels_daily, function(x) {
        #   c(x, rep("", 6))
        # }))
        # 
        # labs = labs[1:(length(breaks_daily))]
        
        # length(labs)
        # length(breaks_daily)
        
        # (mean(d$probe_tt) + sd(d$probe_tt)*3)
        
        
        
        
        # Sometimes there is not enough data to create the scatter plot. Check whether there are enough date points. 
        # There should be at least the same number or just 1 more date points in the scatter data compared to the break points. 
        # The break points start at midnight, so they will be missing the date pointsbefore next day's 12 AM. 
        # That's why I add 1 to break points and compare that with the original data's number of unique date points.
        # Orginal data's unique date poitns should be equal or more commpared to the break point date points.
        # lubridate::date(d$probe_timestamp) %>% unique() %>% length() >= (length(breaks_daily) + 1)
        
        if (wz_duration > 1) {
            tick_check = (lubridate::date(d$probe_timestamp) %>% unique() %>% length() >= length(breaks_daily) )
        } else {
            tick_check = (lubridate::hour(d$probe_timestamp) %>% unique() %>% length() >= length(breaks_daily) )
        }
        
        if ((nrow(d) > 0) & tick_check) {
            g = ggplot() +
                geom_ribbon(data=d, aes(x=probe_timestamp, ymin=travel_time_25th, ymax=travel_time_75th), 
                            fill = "grey69", color="grey48", alpha = 0.5, size = 0.1) +
                geom_point(data=d, aes(x=probe_timestamp, y=probe_tt), color = wz_color, alpha=0.4, size=0.3) +
                # geom_line(data=d, aes(x=probe_timestamp,y=probe_tt), color = "black", alpha= 0.9, size=0.3) +
                scale_y_continuous(limits = c(min(d$probe_tt), max(d$probe_tt)) ) +
                # coord_cartesian(ylim = c(d$travel_time_5th, d$travel_time_85th) ) + 
                # scale_x_date(date_breaks= lubridate::date(d$probe_timestamp) %>% unique(), date_labels=lubridate::date(d$probe_timestamp) %>% unique()) +
                # scale_x_date(date_breaks= lubridate::date(d$probe_timestamp) %>% unique(), date_labels=lubridate::date(d$probe_timestamp) %>% unique()) +
                scale_x_datetime(expand = expand_scale(),
                                 # date_breaks = "3 days" ,
                                 breaks = breaks_daily,
                                 # labels = date_format("%m/%d\n%a"),
                                 labels = labs) +
                # date_minor_breaks = "1 day",
                # limits = c(d$probe_timestamp%>%min(), d$probe_timestamp%>%max())) +
                theme_bw() + 
                theme(#panel.background = element_rect(fill = 'grey97'),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
                    # text = element_text(size=16),
                    axis.text.x = element_text(colour="black", angle=0, hjust=0.5, size=9),
                    axis.text.y = element_text(colour="black", angle=0, hjust=0.5, size=8),
                    legend.position = "none")
        } else {
            g = ggplot()
        }
        
        ggsave(paste0(OUTPUT_DIRECTORY, "/", wz_id, "/", wz_id,"_TT_Scatter_distribution", ".png"), width = 7, height = 2)
    }
    
    # func_create_distribution_scatter_plot(wz_id, wz$wz_start[i], wz$wz_end[i],
    #                                       wz_sql_distribution, OUTPUT_DIRECTORY)
    

    ####################### Functions to draw CFD Plots ### 
    
    func_data_for_peak_scatter_and_cfd_plots = function(wz_sql, wz_sql_prior) {
        
        a <- wz_sql
        b <- wz_sql_prior
        a <- as.data.frame(a)
        b <- as.data.frame(b)
        colnames(a)[1] <- ("date")
        colnames(a)[2] <- ("TT")
        colnames(b)[1] <- ("date")
        colnames(b)[2] <- ("TT")
        
        dummy_x = data.frame(time= numeric(0))
        
        if (nrow(a) > 0) {
            a$time<-"Work Zone"
            e = a$TT
        } else {
            a = cbind(a, dummy_x)
            e = 0
        }
        if (nrow(b) > 0) {
            b$time<-"Prior Year"
            f = b$TT
        } else {
            b = cbind(b, dummy_x)
            f = 0
        }
        
        # First the work zone data, and then the prior year data
        data_cfd <- rbind(a,b)
        
        max_wz <- quantile(e, 0.9)
        max_prior <- quantile(f, 0.9)
        max_val <- max(max_wz, max_prior, 0)
        max_val <- max_val + 5
        
        min_wz <- min(e)
        min_prior <- min(f)
        min_val <- min(min_wz, min_prior, 0)
        min_val <- max(min_val-3, 0.1)
        
        return(list(a, b, min_val, max_val, data_cfd))
    }
    
   
    
    
    func_plot_cfd = function(df, min_val, max_val){
        
        if (df$time %>% unique() %>% length() == 2) {
            # colors_to_use = factor(c('Work Zone', 'Prior Year'), 
            #                        levels = c('Work Zone', 'Prior Year'), 
            #                        ordered=T, 
            #                        labels=c(wz_color, prior_color) )
            
            colors_to_use = c('Work Zone'=wz_color, 'Prior Year'=prior_color)
        } else if (df$time %>% unique() == 'Prior Year') {
            colors_to_use = c(prior_color)
        } else {
            colors_to_use = c(wz_color)
        }
        
        
        gg = ggplot(df, aes(x=TT)) +
            scale_x_continuous(limits = c(min_val, max_val)) +
            stat_ecdf(aes(color = time, linetype = time),
                      geom = "step", size = 2, alpha=0.9) +
            scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), labels=c(0, 25, 50, 75, 100)) + 
            # scale_color_identity() +
            scale_color_manual(values = colors_to_use) +
            theme_bw() +
            theme(# panel.background = element_rect(fill = 'grey97'),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
                text = element_text(size=16),
                axis.text.x = element_text(colour="black", angle=0, hjust=0.5),
                axis.text.y = element_text(colour="black", angle=0, hjust=0.5),
                legend.position="none"
            ) 
        
        return(gg)
    }
    
    func_create_cfd_plot = function(wz_id, OUTPUT_DIRECTORY, wz_sql, wz_sql_prior, cfd_case) {
        
        
        data_for_peak_scatter_plot = func_data_for_peak_scatter_and_cfd_plots(wz_sql, wz_sql_prior)
        min_val = data_for_peak_scatter_plot[[3]]
        max_val = data_for_peak_scatter_plot[[4]]
        data_cfd = data_for_peak_scatter_plot[[5]]
        
        if (nrow(data_cfd) > 0) { 
            g = func_plot_cfd(data_cfd, min_val, max_val)
        } else {
            g = ggplot()
        }
        
        ggsave(paste0(OUTPUT_DIRECTORY, wz_id, "/", wz_id, '_', cfd_case, ".png"), width = 4.8, height = 3)
        
    }
    
    ###### END CFD Plots ########
    
    ### SQL Eye congestion ###
    func_sql_eye = function (table_name, tmc_string, wz_start, wz_end, 
                             wz_sql_distribution, 
                             df_tmc_order, wz_id_value, 
                             wz_speed_limit=70, 
                             threshold_speed_queue=15, 
                             threshold_speed_congestion=45) {
        
        
        
        if (nrow(wz_sql_distribution) < 1) {
            # return (data.frame('probe_hour' = numeric(),
            #                    'wz_hourly_delay'= numeric(),
            #                    'typical_hourly_delay'=numeric(),
            #                    'wz_hourly_congestion'=numeric(),
            #                    'hourly_typical_congestion'=numeric(),
            #                    
            #                    'wz_hourly_delay_weekend'= numeric(),
            #                    'typical_hourly_delay_weekend'=numeric(),
            #                    'wz_hourly_congestion_weekend'=numeric(),
            #                    'hourly_typical_congestion_weekend'=numeric()))
            
            return (list('eye_data_congestion' = NULL, 
                         'eye_data_delay' = NULL, 
                         'max_val_congestion' = NULL, 
                         'max_val_delay' = NULL))
        }
        
        ## length of all tmc segments chosen for work zone to calculate free flow travel time using speed limit
        mm = df_tmc_order %>% filter(wz_id==wz_id_value)
        len_alltmc = abs(max(mm$end_mm) - min(mm$start_mm))
        len_alltmc
        ##### Select work zone travel time and 50th percentile travel time (typical travel time) 
        ss = wz_sql_distribution
        ss = ss %>% dplyr::select(day_of_week, probe_hour, probe_tt, travel_time_50th, probe_timestamp)
        #### calculate base travel time according to speed limit
        ss$base_tt = (len_alltmc/wz_speed_limit)*60
        ss$base_15 = (len_alltmc/threshold_speed_queue)*60
        ss$base_45 = (len_alltmc/threshold_speed_congestion)*60
        ss$wz_congestion = if_else((ss$probe_tt > ss$base_45), 1,0)
        ss$typical_congestion = if_else((ss$travel_time_50th > ss$base_45), 1,0)
        #### calculate work zone delay relative to base travel time
        ss$delay_wz = ss$probe_tt - ss$base_tt
        ## replace negative values with zero 
        ss[,c('delay_wz')] <- replace(ss[,c('delay_wz')], ss[,c('delay_wz')] <0,0)
        #### calculate typical delay relative to base travel time
        ss$typical_delay = ss$travel_time_50th - ss$base_tt
        ## replace negative values with zero 
        ss[,c('typical_delay')] <- replace(ss[,c('typical_delay')], ss[,c('typical_delay')] <0,0)
        ### Convert time series data to aggregated hourly values
        ss$is_workday = 1
        ss$is_workday[ss$day_of_week %in% c('Saturday', 'Sunday')] = 0 # %>% as.numeric()
        
        ss_weekday = ss[ss$is_workday == 1,]
        ss_weekend = ss[ss$is_workday == 0,]
        
        ss_weekday = ss_weekday %>% 
            group_by(probe_hour) %>%
            summarize(wz_hourly_delay = sum(delay_wz)/n(),
                      typical_hourly_delay = sum(typical_delay)/n(),
                      wz_hourly_congestion = (sum(wz_congestion)/n())*100 %>% round(1),
                      hourly_typical_congestion = (sum(typical_congestion)/n())*100 %>% round(1) )
        
        ss_weekend = ss_weekend %>% 
            group_by(probe_hour) %>%
            summarize(wz_hourly_delay_weekend = sum(delay_wz)/n(),
                      typical_hourly_delay_weekend = sum(typical_delay)/n(),
                      wz_hourly_congestion_weekend = (sum(wz_congestion)/n())*100 %>% round(1),
                      hourly_typical_congestion_weekend = (sum(typical_congestion)/n())*100 %>% round(1) )
        
        ss_empty = data.frame(probe_hour = c(0 ,1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 ,20, 21, 22, 23))
        
        ss_congestion = full_join(ss_empty, ss_weekday %>% select(probe_hour, wz_hourly_congestion, hourly_typical_congestion)) %>% 
            full_join(ss_weekend %>% select(probe_hour, wz_hourly_congestion_weekend, hourly_typical_congestion_weekend))
        
        ss_delay = full_join(ss_empty, ss_weekday %>% select(probe_hour, wz_hourly_delay, typical_hourly_delay)) %>% 
            full_join(ss_weekend %>% select(probe_hour, wz_hourly_delay_weekend, typical_hourly_delay_weekend))
        head(ss)
        ### wz_hourly_delay column shows delay values for work zone. 
        ### typical_hourly_delay column shows typical delay that is expected even when work zone is not present
        ### these two columns can be used as new inputes for the eye chart
        
        
        # return(ss)
        
        # Minutes Delay
        x = transpose(ss_delay)
        colnames(x) = x[1,]
        x = x[-1,]
        x$wz = c('wz_hourly_delay', 'typical_hourly_delay','wz_hourly_delay_weekend', 'typical_hourly_delay_weekend')
        x=x %>% select(c('wz', colnames(x[,1:24])))
        max_val_delay = max(x[, c(2:ncol(x))], 5, na.rm = T)
        ss_delay = x
        
        # Percentage congestion
        x = transpose(ss_congestion)
        colnames(x) = x[1,]
        x = x[-1,]
        x$wz = c('wz_hourly_congestion', 'hourly_typical_congestion','wz_hourly_congestion_weekend', 'hourly_typical_congestion_weekend')
        x=x %>% select(c('wz', colnames(x[,1:24])))
        max_val_congestion = max(x[, c(2:ncol(x))], 100, na.rm = T)
        ss_congestion = x
        
        return (list('eye_data_congestion' = ss_congestion, 
                     'eye_data_delay' = ss_delay, 
                     'max_val_congestion' = max_val_congestion, 
                     'max_val_delay' = max_val_delay))
        
    }
    
    
    
    ################################################### functions to get eye_chart data and max_val and plot eye_chart 
    func_data_for_eye_chart = function(d, wz_time) {
        # d=wz_sql_eye_congestion[[i]]
        
        # d <- d[,colSums(is.na(d))<nrow(d)]
        
        d = as.data.table(d)
        x = data.table::transpose(d[, 2:ncol(d), with=F])
        colnames(x) = d$probe_hour %>% as.character()
        
        
        x[, 'wz'] = wz_time
        
        # Find the last column. 
        # Change the positon of the last column to the first column. 
        # (Move wz column to the 1st place)
        x = x[, c(ncol(x), 1:ncol(x)-1), with=F]
        
        x = as.data.frame(x)
        
        max_val = max(x[, c(2:ncol(x))])
        
        return(list(x, max_val))
        
    }
    
    
    
    func_plot_eye_chart = function(x, max_val, case_eye_chart, OUTPUT_DIRECTORY, wz_id) {
        
        # Two cases: Present and Prior
        # The order goes by Current first row, and Prior second row
        # if (nrow(x) == 2) { # both wz and prior year data present
        #     colors_to_use = c('wz' = wz_color, 'prior' = prior_color)
        # } else if (x$wz == 'prior') { # only prior  year data present
        #     colors_to_use = c(prior_color)
        # } else { # only wz data present
        #     colors_to_use = c(wz_color)
        # }
        
        
        
        if (case_eye_chart == 'congestion') {
            file_name_eye_chart = "_eye_congestion"
            
            colors_to_use = c('wz_hourly_congestion' = wz_color, 
                              'hourly_typical_congestion' = prior_color,
                              'wz_hourly_congestion_weekend' = 'dodgerblue1',
                              'hourly_typical_congestion_weekend' = 'darkgoldenrod1')
            
        } else if (case_eye_chart == 'delay') {
            file_name_eye_chart = "_eye_delay"
            
            colors_to_use = c('wz_hourly_delay' = wz_color, 
                              'typical_hourly_delay' = prior_color,
                              'wz_hourly_delay_weekend' = 'dodgerblue1',
                              'typical_hourly_delay_weekend' = 'darkgoldenrod1')
        }
        
        
        if ((!is_null(x))) { 
            g = ggradar2(x, 
                         group.line.width = rep(c(4,3,2,1), each=25) , 
                         group.point.size = 0.1,
                         background.circle.colour = 'white',
                         grid.label.size = 0,
                         axis.label.size = 8,
                         # legend.text.size = 15,
                         grid.mid = max_val/2,
                         grid.max = max_val,
                         grid.line.width = 0.3,
                         axis.line.colour = 'grey',
                         gridline.mid.colour = "grey",
                         gridline.min.colour = "grey",
                         gridline.max.colour = "grey",
                         gridline.max.linetype = 'solid',
                         gridline.min.linetype = "solid", 
                         gridline.mid.linetype = "solid") +
                geom_label(aes(x=0, y=max_val), label = round(max_val, 2), label.size = 1) + 
                geom_label(aes(x=0, y=max_val/2), label = round(max_val/2, 2), label.size = 1) +
                scale_color_manual(values = colors_to_use) + 
                theme(legend.position="none",
                      plot.margin = margin(-0.6, -1.5, -0.6, -1.5, "cm")) %>% try()
        } else {
            g = ggplot()
        }
        
        ggsave(paste0(OUTPUT_DIRECTORY, wz_id, "/", wz_id, file_name_eye_chart, ".png"), width = 4.95, height = 4.95, dpi = 300)
        
        return(g)
    }
    

    
    
    ##### Volcano Start ##############
    ############################################################################################# loop for querying data for year prior volcano R-graph-congestion mile hours
    func_sql_volcano_congestion_prior_year = function(table_name, tmc_string, wz_start, wz_end, threshold_speed_congestion, threshold_speed_queue) {
        
        
        query = paste0("SELECT second_table.tmc,
                   month_id,
                   --queue_mile_hours,
                   congestion_mile_hours,
                   [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen].[TMC_Length] AS tmc_len
                   --[MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen].[start_mm] AS tmc_start_MM
                   FROM(
                   SELECT tmc,
                   month_id,
                   sum(queue_mile_hours) AS queue_mile_hours,
                   SUM(congestion_mile_hours) AS congestion_mile_hours
                   FROM(
                   SELECT 
                   tmc_code1 AS tmc,
                   --DATEPART(HOUR, min_probe) AS time_stamp,
                   DATEPART(Month, min_probe) AS month_id,
                   SUM(CASE WHEN tmcspeed1 < ", threshold_speed_queue, "THEN 5*tmc_len1 ELSE 0 END)*1/60 As queue_mile_hours,
                   SUM(CASE WHEN tmcspeed1 < ", threshold_speed_congestion," AND tmcspeed1 > ", threshold_speed_queue," THEN 5 ELSE 0 END)*1/60 As congestion_mile_hours
                   FROM(
                   SELECT 
                   S.TMC AS tmc_code1,
                   CAST(CONCAT(AVG(DATEPART(YEAR, time_stamp)),'-',
                   AVG(DATEPART(MONTH, time_stamp)),'-',
                   AVG(DATEPART(DAY, time_stamp)),' ',
                   AVG(DATEPART(HOUR, time_stamp)),':',
                   AVG(DATEPART(MINUTE, time_stamp)),':00') AS DATETIME) AS min_Probe,
                   AVG(tmcspeed) AS tmcspeed1,
                   AVG(tmc_len) AS tmc_len1
                   FROM(
                   
                   SELECT
                   first_table.tmc_code AS TMC
                   ,first_table.time_stamp1 AS time_stamp
                   ,first_table.speed AS tmcspeed
                   ,[MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen].[TMC_Length] AS tmc_len
                   
                   FROM (	
                   SELECT *
                   FROM ", table_name ,"
                   WHERE 
                   tmc_code in (", tmc_string ,") AND
                   --datename(weekday, [time_stamp1]) in ('Saturday', 'Sunday') AND 
                   --((DATEPART(dw, [time_stamp1]) + @@DATEFIRST) % 7) NOT IN (0, 1) AND
                   --[confidence] >= '71' AND
                   --[cvalue] = '30' AND
                   [calculated_travel_time] > '0' AND
                   [calculated_travel_time] IS NOT NULL AND
                   [time_stamp1] > '", wz_start, "' AND
                   [time_stamp1] < '", wz_end, "'
                   --DATEPART(HOUR, [time_stamp1])>= '9'AND
                   --DATEPART(HOUR, [time_stamp1])< '10'
                   ) AS first_table
                   left JOIN [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen] ON [first_table].[tmc_code] = [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen].[tmc] ) AS S 
                   GROUP BY 
                   S.TMC,
                   DATEPART(YEAR, time_stamp),
                   DATEPART(MONTH, time_stamp),
                   DATEPART(DAY, time_stamp),
                   DATEPART(HOUR, time_stamp),
                   DATEPART(MINUTE, time_stamp)/5
                   --ORDER BY min_Probe
                   ) AS kk
                   
                   GROUP BY
                   tmc_code1,
                   --DATEPART(HOUR, min_probe),
                   DATEPART(Month, min_probe)
                   )AS dd
                   GROUP BY 
                   tmc,
                   month_id ) AS second_table 
                   left JOIN [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen] ON [second_table].[tmc] = [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen].[tmc]")
        
        # wz_year_prior_sql_volcano_congestion_graph[[i]] 
        wz_sql_volcano_prior_year = sqlQuery(odbChannel, query)
        return (wz_sql_volcano_prior_year)
        
    }
    
    
    ############################################################################################# loop for querying data for wz year volcano R-graph-congestion mile hours
    func_sql_volcano_congestion_wz = function(table_name, tmc_string, wz_start, wz_end, threshold_speed_congestion, threshold_speed_queue) {
        
        query = paste0("SELECT second_table.tmc,
                   --month_id,
                   --queue_mile_hours,
                   congestion_mile_hours,
                   [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen].[TMC_Length] AS tmc_len
                   --[MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen].[start_mm] AS tmc_start_MM
                   FROM(
                   SELECT tmc,
                   --month_id,
                   sum(queue_mile_hours) AS queue_mile_hours,
                   SUM(congestion_mile_hours) AS congestion_mile_hours
                   FROM(
                   SELECT 
                   tmc_code1 AS tmc,
                   --DATEPART(HOUR, min_probe) AS time_stamp,
                   DATEPART(Month, min_probe) AS month_id,
                   SUM(CASE WHEN tmcspeed1 < ",threshold_speed_queue," THEN 5*tmc_len1 ELSE 0 END)*1/60 As queue_mile_hours,
                   SUM(CASE WHEN tmcspeed1 < ",threshold_speed_congestion," AND tmcspeed1 > ",threshold_speed_queue," THEN 5 ELSE 0 END)*1/60 As congestion_mile_hours
                   FROM(
                   SELECT 
                   S.TMC AS tmc_code1,
                   CAST(CONCAT(AVG(DATEPART(YEAR, time_stamp)),'-',
                   AVG(DATEPART(MONTH, time_stamp)),'-',
                   AVG(DATEPART(DAY, time_stamp)),' ',
                   AVG(DATEPART(HOUR, time_stamp)),':',
                   AVG(DATEPART(MINUTE, time_stamp)),':00') AS DATETIME) AS min_Probe,
                   AVG(tmcspeed) AS tmcspeed1,
                   AVG(tmc_len) AS tmc_len1
                   FROM(
                   
                   SELECT
                   first_table.tmc_code AS TMC
                   ,first_table.time_stamp1 AS time_stamp
                   ,first_table.speed AS tmcspeed
                   ,[MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen].[TMC_Length] AS tmc_len
                   
                   FROM (	
                   SELECT *
                   FROM ", table_name ,"
                   WHERE 
                   tmc_code in (", tmc_string ,") AND
                   --datename(weekday, [time_stamp1]) in ('Saturday', 'Sunday') AND 
                   --((DATEPART(dw, [time_stamp1]) + @@DATEFIRST) % 7) NOT IN (0, 1) AND
                   --[confidence] >= '71' AND
                   --[cvalue] = '30' AND
                   [calculated_travel_time] > '0' AND
                   [calculated_travel_time] IS NOT NULL AND
                   [time_stamp1] > '", wz_start, "' AND
                   [time_stamp1] < '", wz_end, "'
                   --DATEPART(HOUR, [time_stamp1])>= '9'AND
                   --DATEPART(HOUR, [time_stamp1])< '10'
                   ) AS first_table
                   left JOIN [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen] ON [first_table].[tmc_code] = [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen].[tmc] ) AS S 
                   GROUP BY 
                   S.TMC,
                   DATEPART(YEAR, time_stamp),
                   DATEPART(MONTH, time_stamp),
                   DATEPART(DAY, time_stamp),
                   DATEPART(HOUR, time_stamp),
                   DATEPART(MINUTE, time_stamp)/5
                   --ORDER BY min_Probe
                   ) AS kk
                   
                   GROUP BY
                   tmc_code1,
                   --DATEPART(HOUR, min_probe),
                   DATEPART(Month, min_probe)
                   )AS dd
                   GROUP BY 
                   tmc
                   --month_id
                   ) AS second_table 
                   left JOIN [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen] ON [second_table].[tmc] = [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen].[tmc]")
        
        dummy_x = data.frame(month_id = numeric(0))
        wz_sql_volcano_congestion_wz = sqlQuery(odbChannel, query)
        
        if ( nrow(wz_sql_volcano_congestion_wz) > 0 ) {
            wz_sql_volcano_congestion_wz['month_id'] = lubridate::month(wz$wz_start[i])
        } else {
            wz_sql_volcano_congestion_wz = cbind(wz_sql_volcano_congestion_wz, dummy_x)
        }
        
        return (wz_sql_volcano_congestion_wz)
    }
    
    
    ################################################################################### func_create_volcano_graph_data
    ##########
    
    func_x_coor = function(len)
    {
        coor_x = list()
        
        len = c(0, len)
        len = cumsum(len)
        
        for (j in seq_len(length(len)-1)) {
            coor_x[[j]] = c(len[j], len[j+1], len[j+1], len[j])
        }
        
        # coor_x = unlist(coor_x)
        
        return(coor_x)
    }
    #####################
    func_create_volcano_graph_data = function(wz_id_value, wz_sql_volcano, df_tmc_order, volcano_data_type) 
    {
        
        # x = wz_sql_volcano_queue_graph[[i]]
        x = wz_sql_volcano
        x$tmc = as.character(x$tmc)
        x = as.data.table(x)
        # str(x)
        
        
        y = df_tmc_order %>% filter(wz_id == wz_id_value) %>% as.data.table()
        
        ##create_volcano_graph = function(x, y){
        # The function takes as input:
        # x: the volcano data table
        # y: the tmc order info table of the work zone.
        # The output: volcano plot is returned.
        
        # full_join(y,x)
        x=merge(x=x, y=y, by='tmc')
        x=x %>%
            arrange(month_id, tmc_order) %>%
            as.data.table()
        
        
        x_month = x %>% 
            split(by = 'month_id')
        
        # Create the x coordinates, which will be the same for all the months.
        # Slect the tmc with maximum number of months avaialble data
        max_num_tmcs = lapply(X=x_month, FUN=nrow) %>% unlist() %>% which.max()
        
        
        coor_x = func_x_coor(x_month[[max_num_tmcs]]$tmc_len)
        # coor_x
        
        
        # Create y coordinates
        # These are the heights of the rectangles aand represent the congestion.
        
        cum_con = list()
        tmc_id = list()
        
        # Get all the values for a certain tmc for all the months
        x
        x = x[order(tmc_order, month_id)]
        x
        split_tmc = split(x, by= 'tmc')
        split_tmc
        
        # Find the maximum number of unique months
        n_months = x$month_id %>% unique() %>% length()
        months = x$month_id %>% unique() %>% sort()
        # Find the tmc missing any month
        lapply(split_tmc, nrow) < n_months
        
        tmcs = split_tmc %>% names()
        tmcs_missing_months = tmcs[lapply(split_tmc, nrow) < n_months]
        
        #In each tmc with missing months, find out which month is missing, and fill that row
        tmcs_missing_months
        
        if (length(tmcs_missing_months) > 0) {
            for (tmc_code in tmcs_missing_months) {
                data_missing_month = split_tmc[[tmc_code]]
                # data_missing_month = data_missing_month[c(1,4,5,6,7),]
                missing_months = months[!months %in% data_missing_month[, (month_id)]]
                # print(missing_months)
                data_missing_month_filled = data.table(tmc = tmc_code, 
                                                       month_id = missing_months, 
                                                       congestion_mile_hours = NaN, 
                                                       tmc_len = data_missing_month$tmc_len[1],
                                                       wz_id = data_missing_month$wz_id[1],
                                                       start_mm = data_missing_month$start_mm[1],
                                                       end_mm = data_missing_month$end_mm[1],
                                                       tmc_order = data_missing_month$tmc_order[1]
                )
                data_missing_month = rbind(data_missing_month, data_missing_month_filled) %>% arrange(month_id) %>% as.data.table()
                data_missing_month$congestion_mile_hours = na.locf(data_missing_month$congestion_mile_hours, na.rm = F, fromLast = T)
                data_missing_month$congestion_mile_hours = na.locf(data_missing_month$congestion_mile_hours, na.rm = F, fromLast = F)
                split_tmc[[tmc_code]] = data_missing_month
            }
        }
        
        
        for (i in seq_along(split_tmc)) {
            if (volcano_data_type == 'congestion') {
                cum_con[[i]] = cumsum(append(split_tmc[[i]]$congestion_mile_hours, 0, after=0))
            } else if (volcano_data_type == 'queue') {
                cum_con[[i]] = cumsum(append(split_tmc[[i]]$queue_mile_hours, 0, after=0))
            }
            tmc_id[[i]] = split_tmc[[i]]$tmc
        }
        
        
        tmc = list()
        coor_y = list()
        coor_x_new = list()
        month_id = list()
        for (j in seq_len(length(cum_con))) {
            v = cum_con[[j]]
            coor_y[[j]] = list()
            coor_x_new[[j]] = list()
            month_id[[j]] = list()
            tmc[[j]] = list()
            
            for (i in seq_len(length(v)-1)) {
                
                coor_y[[j]][[i]] = c(v[i], v[i], v[i+1], v[i+1])
                coor_x_new[[j]][[i]] = coor_x[[j]]
                
                month_id[[j]][[i]] = rep(i, 4)
                tmc[[j]][[i]] = rep(tmc_id[[j]][[i]],4)
            }
            
        }
        
        dt = data.frame(x=unlist(coor_x_new), 
                        y=unlist(coor_y), 
                        month_id=unlist(month_id), 
                        tmc=unlist(tmc))
        dt$tmc = as.character(dt$tmc)
        dt = dt %>% as.data.table()
        
        df_tmc_start_mm = y %>% select(tmc, start_mm, tmc_order)
        dt = merge(dt, df_tmc_start_mm, by.x='tmc', by.y='tmc')
        
        # unique(dt$start_mm)
        # dt[, .(unique(start_mm)), by=tmc]
        
        # dt %>% arrange(month_id, tmc_order,x,y)
        
        
        dt = unite(dt, col='tmc_month', c('tmc', 'month_id'), remove = F)
        
        
        # start_mm = dt[, .(start_mm = unique(start_mm)), by = .(tmc)][order(start_mm, decreasing=F)][, start_mm]
        
        # x_ticks = dt$x %>% unique() %>% sort()
        # x_ticks = c(sort(dt$x)[seq(2, length(dt$x), 4)], tail(sort(dt$x),1))
        # start_mm = c(sort(dt$start_mm)[seq(1, length(dt$start_mm), 4)])
        
        
        x_ticks = dt[, .(x = unique(x)), keyby = .(tmc_order)][order(tmc_order, x), ]
        last_tick = tail(x_ticks$x,1)
        x_ticks = c(x_ticks[, .(x = head(x, 1)), keyby=.(tmc_order)][, x], last_tick)
        
        start_mm = dt[, .(x = unique(start_mm)), keyby = .(tmc_order)][order(tmc_order, x), ][,x]
        
        if (length(x_ticks) != length(start_mm)) {
            start_mm = c(start_mm, tail(start_mm, 1) + x_ticks[length(x_ticks)] - x_ticks[length(x_ticks)-1] )
        }
        
        
        # start_mm = start_mm %>% round(1)
        # 
        # return(list(dt, x_ticks2, start_mm))
        
        # x_ticks2 = ceiling(x_ticks)
        # x_ticks2 = seq(from=head(x_ticks2,1), to=tail(x_ticks2,1), by=1 ) 
        
        start_mm2 = ceiling(start_mm)
        # difference = start_mm2 - start_mm
        start_mm2 = seq(from=head(start_mm2,1), to=tail(start_mm2,1), by=1 ) 
        
        
        x_ticks2 = seq(0, length(start_mm2)-1, by=1)
        x_ticks2 = x_ticks2 + (start_mm2[1] - start_mm[1])
        # x_ticks2 = x_ticks2 + difference
        # head(x_ticks2, length(start_mm2))
        
        # start_mm3 = seq(from=start_mm[1], by=1, length.out = length(x_ticks2))
        
        work_zone_start_mm = start_mm[1]
        
        return(list(dt, x_ticks2, start_mm2, work_zone_start_mm))
        
    }
    
    
    func_create_volcano_plot = function(volcano_graph_data, wz_Start_MM, wz_End_MM, case_volcano_chart) {
        
        dt = volcano_graph_data[[1]]
        x_ticks = volcano_graph_data[[2]]
        start_mm = volcano_graph_data[[3]]
        work_zone_start_mm = volcano_graph_data[[4]]
        
        # To signify the start and end of a work zone
        wz_start_mm2 =  wz_Start_MM - work_zone_start_mm[1]
        wz_end_mm2 = wz_End_MM - work_zone_start_mm[1]
        
        
        if (case_volcano_chart == 'wz') {
            colors_to_use = 'orchid'
        } else if (case_volcano_chart == 'prior') {
            colors_to_use = c("dodgerblue2","dodgerblue3","springgreen","green3","forestgreen","yellow","gold","goldenrod1","darkorange","darkorange2","darkorange4","dodgerblue1")
        }
        
        
        g = ggplot(dt, aes(x=y, y=x)) +
            geom_polygon(aes(fill=as.factor(month_id), group=tmc_month)) +
            geom_hline(yintercept = c( wz_start_mm2, wz_end_mm2), size=1, color='black') + 
            # scale_fill_manual('Legend', labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
            #                   values = c("dodgerblue2","dodgerblue3","springgreen","green3","forestgreen","yellow","gold","goldenrod1","darkorange","darkorange2","darkorange4","dodgerblue1","black", "black", "black", "black", "black","black", "black", "black", "black", "black","black", "black", "black", "black", "black")
            # ) +
            scale_fill_manual('Legend', labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                              values = colors_to_use
            ) +
            scale_y_continuous(expand = expand_scale(), breaks = x_ticks, labels = start_mm) +
            scale_x_continuous(expand = expand_scale()) + 
            theme(
                #panel.background = element_rect(fill = 'grey97'),
                #     legend.title = 'bk',
                #    axis.title.x=element_blank(),
                #   axis.title.y=element_blank(),
                axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
                text = element_text(size=15),
                axis.text.x = element_text(colour="black", angle=0, hjust=1, size=20),
                axis.text.y = element_text(colour="black", angle=0, hjust=0.5, size=18),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                # legend.position="right",
                legend.position="none",
                panel.background = element_rect(fill = NA),
                ##,legend.box = "horizontal",
                legend.text = element_text(size = 20),
                panel.border = element_rect(colour = "black", fill=NA, size=0.5)
            )
        return(g)
    }
    
    
    func_sql_max_queue_length = function(table_name, tmc_string, wz_start, wz_end) {
        
        query = paste0("SELECT
                   CAST(CONCAT(AVG(DATEPART(YEAR, min_Probe)),'-',
                   AVG(DATEPART(MONTH, min_Probe)),'-',
                   AVG(DATEPART(DAY, min_Probe)),' ',
                   AVG(DATEPART(HOUR, min_Probe)),':',
                   AVG(DATEPART(MINUTE, min_Probe)),':00') AS DATETIME) AS min_Probe,
                   SUM(CASE WHEN Avgspeed_PROBE < 20 THEN tmc_len ELSE 0 END) AS queue_len
                   FROM(
                   SELECT 
                   second_table.tmc_code, 
                   second_table.min_Probe,
                   second_table.Avgspeed_PROBE,
                   [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen].[TMC_Length] AS tmc_len
                   FROM(
                   SELECT 
                   tmc_code,
                   CAST(CONCAT(AVG(DATEPART(YEAR, [time_stamp1])),'-',
                   AVG(DATEPART(MONTH, [time_stamp1])),'-',
                   AVG(DATEPART(DAY, [time_stamp1])),' ',
                   AVG(DATEPART(HOUR, [time_stamp1])),':',
                   AVG(DATEPART(MINUTE, [time_stamp1])),':00') AS DATETIME) AS min_Probe,
                   
                   CAST(CONCAT(AVG(DATEPART(YEAR, [time_stamp1])),'-',
                   AVG(DATEPART(MONTH, [time_stamp1])),'-',
                   AVG(DATEPART(DAY, [time_stamp1])),' ',
                   AVG(DATEPART(HOUR, [time_stamp1])),':00') AS DATETIME) AS hr_Probe,
                   AVG([speed]) AS Avgspeed_PROBE,
                   AVG([calculated_travel_time]) AS TT_PROBE,
                   COUNT ([tmc_code]) as Count_Probe
                   FROM (
                   SELECT *
                   FROM ", table_name ,"
                   WHERE 
                   tmc_code in (", tmc_string ,") AND
                   [calculated_travel_time] > '0' AND
                   [calculated_travel_time] IS NOT NULL AND
                   [time_stamp1] > '", wz_start, "' AND
                   [time_stamp1] < '", wz_end, "'
                   --datename(weekday, [time_stamp1]) in ('Saturday', 'Sunday') AND
                   --((DATEPART(dw, [time_stamp1]) + @@DATEFIRST) % 7) NOT IN (0, 1) AND
                   --DATEPART(HOUR, [time_stamp1])>= '6'AND
                   --DATEPART(HOUR, [time_stamp1])< '10'
                   ) AS first_table 
                   GROUP BY
                   DATEPART(YEAR, [time_stamp1]),
                   DATEPART(MONTH, [time_stamp1]),
                   DATEPART(DAY, [time_stamp1]),
                   DATEPART(HOUR, [time_stamp1]),
                   (DATEPART(MINUTE, [time_stamp1])/5),
                   tmc_code ) AS second_table
                   left JOIN [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen] ON second_table.[tmc_code] = [MI_Mobility].[dbo].[INRIX_Interstate_MileMarker_Mohsen].[tmc] ) AS third_table
                   GROUP BY
                   DATEPART(YEAR, min_Probe),
                   DATEPART(MONTH, min_Probe),
                   DATEPART(DAY, min_Probe),
                   DATEPART(HOUR, min_Probe),
                   (DATEPART(MINUTE, min_Probe)/5) ")
        
        
        
        wz_sql_max_queue_length = sqlQuery(odbChannel, query)
        return(wz_sql_max_queue_length)
        
    }
    
    
    ############################################################################################# Statistics - loop for querying maximum queue length
    
    
    ############################ getting summary statistics
    # setwd("C:/R")
    
    
    func_summarize = function(wz, i, 
                              wz_sql_distribution, wz_sql_max_queue_length) {
        
        wz_id = wz$wz_id[i]
        county = wz$county[i]
        road = wz$road[i]
        category = wz$category[i]
        wz_start = wz$wz_start[i]
        wz_end = wz$wz_end[i]
        start_mm = wz$start_mm[i]
        end_mm = wz$end_mm[i]
        Direction = wz$direction[i]
        
        # wz_am_prior_lottr = as.data.frame(wz_sql_am_prior_lottr)[1,]
        # wz_mid_prior_lottr = as.data.frame(wz_sql_mid_prior_lottr)[1,]
        # wz_pm_prior_lottr = as.data.frame(wz_sql_pm_prior_lottr)[1,]
        # wz_weekend_prior_lottr = as.data.frame(wz_sql_weekend_prior_lottr)[1,]
        wz_statistics <- as.data.frame(wz_sql_distribution)
        
        
        if (nrow(wz_statistics) > 0) {
            
            
            
            ## length of all tmc segments chosen for work zone to calculate free flow travel time using speed limit
            mm = df_tmc_order
            len_alltmc = abs(max(mm$end_mm) - min(mm$start_mm))
            len_alltmc
            
            ##### new summary statistic approach - delay and LOTTR metrics
            delay_new <- wz_sql_distribution
            delay_new = delay_new %>% dplyr::select(probe_hour,day_of_week,probe_tt,travel_time_50th, probe_timestamp)
            ####
            delay_new['delay'] = delay_new['probe_tt'] - delay_new['travel_time_50th']
            delay_new[,c('delay')] <- replace(delay_new[,c('delay')], delay_new[,c('delay')] <0,0)
            ### filter records for morning time period
            delay_am <- delay_new %>% filter(probe_hour %in% c('6','7','8','9') &
                                                 day_of_week %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
            ### filter records for mid day time period
            delay_mid <- delay_new %>% filter(probe_hour %in% c('10','11','12','13','14','15') &
                                                  day_of_week %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
            ### filter records for evening time period
            delay_pm <- delay_new %>% filter(probe_hour %in% c('16','17','18','19') &
                                                 day_of_week %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
            ### filter records for weekend time period
            delay_weekend <- delay_new %>% filter(probe_hour %in% c('6','7','8','9','10','11','12','13','14','15','16','17','18','19') &
                                                      day_of_week %in% c('Saturday', 'Sunday'))
            ### delay metrics in am period
            if (nrow(delay_am)>0) {
                avg_am_delay <- median(delay_am$delay)
                max_am_delay <- max(delay_am$delay)
                total_am_delay <- sum(delay_am$delay)
                wz_am_lottr <- quantile(delay_am$probe_tt, probs = 0.85)[[1]]/quantile(delay_am$probe_tt, probs = 0.50)[[1]]
            } else {
                avg_am_delay <- NA
                max_am_delay <- NA
                total_am_delay <- NA
                wz_am_lottr <- NA
            }
            
            ### delay metrics in mid period
            if (nrow(delay_mid)>0) {
                avg_mid_delay <- median(delay_mid$delay)
                max_mid_delay <- max(delay_mid$delay)
                total_mid_delay <- sum(delay_mid$delay)
                wz_mid_lottr <- quantile(delay_mid$probe_tt, probs = 0.85)[[1]]/quantile(delay_mid$probe_tt, probs = 0.50)[[1]]
            } else {
                avg_mid_delay <- NA
                max_mid_delay <- NA
                total_mid_delay <- NA
                wz_mid_lottr <- NA
            }
            
            ### delay metrics in pm period
            if (nrow(delay_pm)>0) {
                avg_pm_delay <- median(delay_pm$delay)
                max_pm_delay <- max(delay_pm$delay)
                total_pm_delay <- sum(delay_pm$delay)
                wz_pm_lottr <- quantile(delay_pm$probe_tt, probs = 0.85)[[1]]/quantile(delay_pm$probe_tt, probs = 0.50)[[1]]
            } else {
                avg_pm_delay <- NA
                max_pm_delay <- NA
                total_pm_delay <- NA
                wz_pm_lottr <- NA
            }
            
            ### delay metrics in weekend period
            if (nrow(delay_weekend)>0) {
                avg_weekend_delay <- median(delay_weekend$delay)
                max_weekend_delay <- max(delay_weekend$delay)
                total_weekend_delay <- sum(delay_weekend$delay)
                wz_weekend_lottr <- quantile(delay_weekend$probe_tt, probs = 0.85)[[1]]/quantile(delay_weekend$probe_tt, probs = 0.50)[[1]]
            } else {
                avg_weekend_delay <- NA
                max_weekend_delay <- NA
                total_weekend_delay <- NA
                wz_weekend_lottr <- NA
            }
            ### Overal metrics
            avg_wz_delay <- median(delay_new$delay)
            max_wz_delay <- max(delay_new$delay)
            total_wz_delay <- sum(delay_new$delay)
            wz_lottr <- quantile(delay_new$probe_tt, probs = 0.85)[[1]]/quantile(delay_new$probe_tt, probs = 0.50)[[1]]
            ### ################################  queue length
            wz_queue <- as.data.frame(wz_sql_max_queue_length)
            max_queue_length <- max(wz_queue[,2])
            #num_queue <- nrow(wz_queue %>%
            #                    filter(wz_queue$queue_len > 0))
            
            
            ################################  queue duration
            #total_queue_duration <- num_queue*5
            setDT(wz_queue)
            wz_queue <- wz_queue[order(wz_queue$min_Probe)]
            ####
            wz_queue[, queue_presence := +(wz_queue$queue_len > 0)
                     ][, queue_duration := with(rle(queue_presence), rep(lengths,lengths))
                       ][queue_presence == 0, queue_duration := 0]
            ####
            wz_queue[, queue_lag := +(wz_queue$queue_len == 0)
                     ][, queue_lag_duration := with(rle(queue_lag), rep(lengths,lengths))
                       ][queue_lag == 0, queue_lag_duration := 0]
            ####
            wz_queue <- wz_queue %>% mutate(queue_presence_modified =ifelse(wz_queue$queue_lag_duration == 1,1,wz_queue$queue_presence))
            ####
            setDT(wz_queue)
            wz_queue <- wz_queue[order(wz_queue$min_Probe)]
            ####
            wz_queue[, queue_presence_modified1 := +(wz_queue$queue_presence_modified == 1)
                     ][, queue_duration_modified := with(rle(queue_presence_modified1), rep(lengths,lengths))
                       ][queue_presence_modified1 == 0, queue_duration_modified := 0]
            ####
            wz_queue <- wz_queue %>% mutate(diff = queue_presence_modified1-lag(queue_presence_modified1))
            ###
            wz_queue_formation <- wz_queue %>% filter(wz_queue$diff ==1)
            num_queue <- nrow(wz_queue_formation)
            total_queue_duration <- nrow(wz_queue %>% filter(wz_queue$queue_presence_modified1 ==1))*5              
            max_queue_duration <- max(wz_queue$queue_duration_modified)*5  
            ###################### get the metrics into a table 
            wz_summary_table = data.table(
                'wz_id' = wz_id,
                'avg_wz_delay' = avg_wz_delay,
                'max_wz_delay' = max_wz_delay,
                'total_wz_delay' = total_wz_delay,
                'avg_am_delay' = avg_am_delay,
                'max_am_delay' = max_am_delay,
                'total_am_delay' = total_am_delay,
                'wz_am_lottr' = wz_am_lottr,
                'avg_mid_delay' = avg_mid_delay,
                'max_mid_delay' = max_mid_delay,
                'total_mid_delay' = total_mid_delay,
                'wz_mid_lottr' = wz_mid_lottr,
                'avg_pm_delay' = avg_pm_delay,
                'max_pm_delay' = max_pm_delay,
                'total_pm_delay' = total_pm_delay,
                'wz_pm_lottr' = wz_pm_lottr,
                'avg_weekend_delay' = avg_weekend_delay,
                'max_weekend_delay' = max_weekend_delay,
                'total_weekend_delay' = total_weekend_delay,
                'wz_weekend_lottr' = wz_weekend_lottr,
                'max_queue_length' = max_queue_length,
                'num_queue' = num_queue,
                'total_queue_duration' = total_queue_duration,
                'max_queue_duration' = max_queue_duration,
                'wz_lottr' = wz_lottr)
            
            # print(df)
            
            
            
            wz_delay_metrics <- data.frame('Stats' = c('Avg Delay(min)', 'Max Delay(min)', 'Total Delay(min)', 'LOTTR'),
                                           'AM' = c(avg_am_delay, max_am_delay, total_am_delay, wz_am_lottr),
                                           'Mid' = c(avg_mid_delay, max_mid_delay, total_mid_delay, wz_mid_lottr),
                                           'PM' = c(avg_pm_delay, max_pm_delay, total_pm_delay, wz_pm_lottr),
                                           'Weekend' = c(avg_weekend_delay, max_weekend_delay, total_weekend_delay, wz_weekend_lottr),
                                           'Total' = c(avg_wz_delay, max_wz_delay, total_wz_delay, wz_lottr)
            )
            
            wz_queue_metrics <- data.frame('Stats' = c('Max Duration(min)', 'Total Duration(min)', 'Max Length(miles)', '# of Queues'),
                                           'Queue' = c(max_queue_duration, total_queue_duration, max_queue_length, num_queue)
            )
            wz_information <- data.frame('Overview' = c('Work Zone ID','County','Roadway','Closure type','Direction','Start Milemarker','End Milemarker'),
                                         'Information' = c(wz_id, county, road, category, Direction, round(start_mm,1), round(end_mm,1)))
            
            wz_information_date <- data.frame('Overview' = c('Workzone Start', 'Workzone End'),
                                              'Information' = c(wz_start, wz_end) %>% as.character() %>% str_sub(end=-4))
            
            wz_information = rbind(wz_information, wz_information_date)
            
        } else {
            
            wz_summary_table = data.table(
                'wz_id' = wz_id,
                'avg_wz_delay' = NA,
                'max_wz_delay' = NA,
                'total_wz_delay' = NA,
                'avg_am_delay' = NA,
                'max_am_delay' = NA,
                'total_am_delay' = NA,
                'wz_am_lottr' = NA,
                'avg_mid_delay' = NA,
                'max_mid_delay' = NA,
                'total_mid_delay' = NA,
                'wz_mid_lottr' = NA,
                'avg_pm_delay' = NA,
                'max_pm_delay' = NA,
                'total_pm_delay' = NA,
                'wz_pm_lottr' = NA,
                'avg_weekend_delay' = NA,
                'max_weekend_delay' = NA,
                'total_weekend_delay' = NA,
                'wz_weekend_lottr' = NA,
                'max_queue_length' = NA,
                'num_queue' = NA,
                'total_queue_duration' = NA,
                'max_queue_duration' = NA,
                'wz_lottr' = NA)
            
            
            wz_delay_metrics <- data.frame('Stats' = c('Avg Delay(min)', 'Max Delay(min)', 'Total Delay(min)', 'LOTTR'),
                                           'AM' = c(NaN,NaN,NaN,NaN),
                                           'Mid' = c(NaN,NaN,NaN,NaN),
                                           'PM' = c(NaN,NaN,NaN,NaN),
                                           'Weekend' = c(NaN,NaN,NaN,NaN),
                                           'Total' = c(NaN,NaN,NaN,NaN)
            )
            
            wz_queue_metrics <- data.frame('Stats' = c('Max Duration(min)', 'Total Duration(min)', 'Max Length(miles)', '# of Queues'),
                                           'Queue' = c(NaN,NaN,NaN,NaN)
            )
            wz_information <- data.frame('Overview' = c('Work Zone ID','County','Roadway','Closure type','Direction','Start Milemarker','End Milemarker'),
                                         'Information' = c(wz_id, county, road, category, Direction, round(start_mm,1), round(end_mm,1)))
            
            wz_information_date <- data.frame('Overview' = c('Workzone Start', 'Workzone End'),
                                              'Information' = c(wz_start, wz_end) %>% as.character() %>% str_sub(end=-4))
            
            wz_information = rbind(wz_information, wz_information_date)
        } 
        
        return (list('wz_summary_table' = wz_summary_table, 
                     'wz_delay_metrics' = wz_delay_metrics, 
                     'wz_queue_metrics' = wz_queue_metrics, 
                     'wz_information'   = wz_information))
    }
    
    ####################################### Filling in the two-page summary #####################################
    
    
    func_write_report = function(wz_id, summary, OUTPUT_DIRECTORY, path_ppt_file) {
        
        problematic_wz = c()
        
        wz_delay_metrics = summary$wz_delay_metrics
        wz_queue_metrics = summary$wz_queue_metrics
        wz_information = summary$wz_information
        
        wz_delay_metrics_ppt = copy(wz_delay_metrics)
        wz_delay_metrics_ppt[] <- lapply(wz_delay_metrics, function(.col){ 
            if (is.numeric(.col)) return(sprintf("%.1f", .col)) 
            else return(.col) 
        })
        
        #wz_delay_metrics_ppt[4, 'Total'] = 'NA'
        
        
        wz_queue_metrics_ppt = copy(wz_queue_metrics)
        wz_queue_metrics_ppt[] <- lapply(wz_queue_metrics, function(.col){ 
            if (is.numeric(.col)) return(sprintf("%.1f", .col)) 
            else return(.col) 
        })
        
        wz_information_ppt = copy(wz_information)
        wz_information_ppt[] <- lapply(wz_information, function(.col){ 
            if (is.numeric(.col)) return(sprintf("%.1f", .col)) 
            else return(.col) 
        })
        
        wz_delay_metrics_ppt <- flextable(wz_delay_metrics_ppt)
        wz_delay_metrics_ppt <- width(wz_delay_metrics_ppt, j = ~ Stats,width=1.3)
        wz_delay_metrics_ppt <- height_all(wz_delay_metrics_ppt,height=0.27)
        wz_delay_metrics_ppt <- align(wz_delay_metrics_ppt, align = "center", part = "all" )
        
        wz_queue_metrics_ppt <- flextable(wz_queue_metrics_ppt)
        wz_queue_metrics_ppt <- width(wz_queue_metrics_ppt, j = ~ Stats,width=1.33)
        wz_queue_metrics_ppt <- height_all(wz_queue_metrics_ppt,height=0.27)
        wz_queue_metrics_ppt <- align(wz_queue_metrics_ppt, align = "center", part = "all" )
        
        wz_information_ppt <- flextable(wz_information_ppt)
        wz_information_ppt <- width(wz_information_ppt,width=1.7)
        wz_information_ppt <- height_all(wz_information_ppt,height=0.21)
        wz_information_ppt <- align(wz_information_ppt, align = "center", part = "all" )
        
        my_pres<- read_pptx(path_ppt_file)
        ################################## Set directory for each WZ to grab the graphs
        
        output_folder = paste0(OUTPUT_DIRECTORY, wz_id, "/", wz_id)
        if (file.exists(paste0(output_folder, '_TT_Scatter_distribution.png')) & 
            file.exists(paste0(output_folder, '_TT_CFD_am.png')) &
            file.exists(paste0(output_folder, '_TT_CFD_mid.png')) &
            file.exists(paste0(output_folder, '_TT_CFD_pm.png')) &
            file.exists(paste0(output_folder, '_TT_CFD_Weekend.png')) &
            file.exists(paste0(output_folder, '_TT_volcano_congestion.png')) &
            file.exists(paste0(output_folder, '_eye_congestion.png')) &
            file.exists(paste0(output_folder, '_eye_delay.png')) &
            file.exists(paste0(output_folder, '_TT_heatmap_speed.png')) &
            file.exists(paste0(output_folder, '_TT_year_prior_volcano_congestion.png'))
            
        ) {
            my_pres %>%
                on_slide(index = 1) %>%
                ph_with_img_at(src = paste0(output_folder, "_TT_Scatter_distribution.png"),left = 0.27, top = 2.99, width = 7.23, height = 2.16) %>%
                ph_with_img_at(src = paste0(output_folder, "_TT_heatmap_speed.png"),left = 0.25, top = 5.58, width = 7.23, height = 2.16) %>%
                #ph_with_img_at(src = paste0(output_folder, "_TT_Scatter_Mid_Prior.png"),left = 1.37, top = 4.41, width = 2.15, height = 1.19)%>%
                #ph_with_img_at(src = paste0(output_folder, "_TT_Scatter_Mid.png"),left = 3.56, top = 4.41, width = 2.15, height = 1.19)%>%
                #ph_with_img_at(src = paste0(output_folder, "_TT_Scatter_PM_Prior.png"),left = 1.37, top = 5.63, width = 2.15, height = 1.19)%>%
                #ph_with_img_at(src = paste0(output_folder, "_TT_Scatter_PM.png"),left = 3.56, top = 5.63, width = 2.15, height = 1.19)%>%
                #ph_with_img_at(src = paste0(output_folder, "_TT_Scatter_Weekend_Prior.png"),left = 1.37, top = 6.87, width = 2.15, height = 1.19)%>%
                #ph_with_img_at(src = paste0(output_folder, "_TT_Scatter_Weekend.png"),left = 3.56, top = 6.87, width = 2.15, height = 1.19)%>%
                ph_with_img_at(src = paste0(output_folder, "_TT_CFD_am.png"),left = 0.2, top = 8.46, width = 1.7, height = 1.32) %>%
                ph_with_img_at(src = paste0(output_folder, "_TT_CFD_mid.png"),left = 2.04, top = 8.46, width = 1.7, height = 1.32) %>%
                ph_with_img_at(src = paste0(output_folder, "_TT_CFD_pm.png"),left = 3.88, top = 8.46, width = 1.7, height = 1.32) %>%
                ph_with_img_at(src = paste0(output_folder, "_TT_CFD_Weekend.png"),left = 5.72, top = 8.46, width = 1.7, height = 1.32) %>%
                ph_with_img_at(src = paste0(output_folder, "_geo_map.png"),left = 3.53, top = 0, width = 3.93, height = 2.64) %>%
                ph_with_flextable_at(wz_information_ppt, left = 0.07, top = 0.44) %>%
                on_slide(index = 2)%>%
                ph_with_img_at(src = paste0(output_folder, "_TT_volcano_congestion.png"),left = 0.28, top = 0.5, width = 3.13, height = 3.44) %>%
                ph_with_img_at(src = paste0(output_folder, "_TT_year_prior_volcano_congestion.png"),left = 3.77, top = 0.5, width = 3.13, height = 3.44) %>%
                ph_with_img_at(src = paste0(output_folder, "_eye_congestion.png"),left = 0.08, top = 4.81, width = 3.05, height = 3.05) %>%
                ph_with_img_at(src = paste0(output_folder, "_eye_delay.png"),left = 4.35, top = 4.81, width = 3.05, height = 3.05) %>%
                ph_with_flextable_at(wz_delay_metrics_ppt, left = 0.03, top = 8.45) %>%
                ph_with_flextable_at(wz_queue_metrics_ppt, left = 5.31, top = 8.45)
            # ph_with_table_at(value =wz_delay_metrics_ppt,
            #                  height = 1.5, width = 5, left = 0, top = 8.47,
            #                  last_row = FALSE, last_column = FALSE, first_row = TRUE)%>%
            # ph_with_table_at(value =wz_queue_metrics_ppt,
            #                  height = 1.5, width = 3, left = 4.5, top = 8.47,
            #                  last_row = FALSE, last_column = FALSE, first_row = TRUE) %>%
            
            
            my_pres %>%
                print( target = paste0(output_folder, "_WZMA.pptx")) %>% 
                invisible()
            
            my_pres %>%
                print( target = paste0(OUTPUT_DIRECTORY, "Reports/", wz_id, "_WZMA.pptx")) %>% 
                invisible()
        } else {
            problematic_wz = c(problematic_wz, wz_id)
            
        }
        
    }
    
    #### BIG LOOP ##############
    
    time_taken_indiv = c()
    # wz_summary_table = list()
    
    # for (i in 876:876) {
    # for (i in 322:nrow(wz)) {
    for (i in seq_len(nrow(wz))) {
        
        
        rm(summary, wz_summary_table_write, wz_sql_scatter, wz_sql_distribution, wz_sql_eye, wz_sql_heatmap, 
           # wz_sql_am_prior_lottr, wz_sql_mid_prior_lottr, wz_sql_pm_prior_lottr, wz_sql_weekend_prior_lottr, 
           wz_sql_max_queue_length, wz_sql_volcano_congestion_prior_year, wz_sql_volcano_congestion_wz)
        
        
        start.time <- Sys.time()
        
        wz_start = wz$wz_start[i]
        wz_end = wz$wz_end[i]
        
        wz_start_prior = wz$wz_prior_start[i]
        wz_end_prior = wz$wz_prior_end[i]
        
        # wz_start_lottr = wz$wz_prior_year_start[i]
        # wz_end_lottr = wz$wz_prior_year_end[i]
        # 
        wz_prior_year_start = wz$wz_prior_year_start[i]
        wz_prior_year_end = wz$wz_prior_year_end[i]
        
        wz_tmc = wz$tmc[i] %>% as.character()
        wz_id = wz$wz_id[i]
        tmc_count = wz$tmc_count[i]
        tmc_string = func_get_tmc_string(wz_tmc)
        
        wz_id_value = wz_id 
        
        print(paste0("Running work zone with ID: ", wz_id_value))
        
        # wz_speed = wz$speed[i]
        
        # wz_Start_MM = wz$start_mm[i]
        # wz_End_MM = wz$end_mm[i]
        
        segments_shp
        # Get the geo-map
        func_create_map(wz_id, wz_tmc, OUTPUT_DIRECTORY, segments_shp, counties_shp)
        
        # Heatmap
        wz_sql_heatmap = func_sql_heatmap(table_name_wz, tmc_string, wz_id_value, wz_start, wz_end, df_tmc_order)
        func_create_heatmap_plot(wz_id, wz_tmc, wz_start, wz_end, tmc_count, wz$start_mm[i], wz$end_mm[i], wz_sql_heatmap)
        
        # Scatter
        wz_sql_scatter = func_sql_scatter(table_name_wz, tmc_string, wz_start, wz_end, tmc_count) 
        # wz_sql_scatter_prior = func_sql_scatter(table_name_prior, tmc_string, wz_start_prior, wz_end_prior, tmc_count) 
        
        # Distribution
        wz_sql_prior_distribution = func_sql_distribution(table_name_prior, tmc_string, wz_prior_year_start, wz_prior_year_end, 
                                                          tmc_count, wz_sql_scatter)
        
        wz_sql_distribution = as.data.frame(merge(wz_sql_scatter, wz_sql_prior_distribution, 
                                                  by= c("day_of_week", "probe_hour", "probe_minute", "probe_season")))
        
        func_create_distribution_scatter_plot(wz_id, wz$wz_start[i], wz$wz_end[i], 
                                              wz_sql_distribution, OUTPUT_DIRECTORY)
        
        ##### CFD new approach
        cfd_new = wz_sql_distribution %>% 
            dplyr::select(probe_timestamp, probe_tt, day_of_week, probe_hour, probe_minute, travel_time_50th)
        cfd_am <- cfd_new %>% filter(probe_hour %in% c('6','7','8','9') &
                                         day_of_week %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
        cfd_mid <- cfd_new %>% filter(probe_hour %in% c('10','11','12','13','14','15') &
                                          day_of_week %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
        cfd_pm <- cfd_new %>% filter(probe_hour %in% c('16','17','18','19') &
                                         day_of_week %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
        cfd_weekend <- cfd_new %>% filter(probe_hour %in% c('6','7','8','9','10','11','12','13','14','15','16','17','18','19') &
                                              day_of_week %in% c('Saturday', 'Sunday'))
        
        ### Start CFD Plots ###
        # wz_start_lottr = wz$wz_prior_year_start[i]
        # wz_end_lottr = wz$wz_prior_year_end[i]
        
        
        #####################
        # Morning Peak
        hour_begin = 6
        hour_end = 10 
        
        ## Current Year
        # wz_sql_am = func_sql_peak_hours(table_name=table_name_wz, hour_begin, hour_end, wz_start, wz_end, tmc_string, tmc_count)
        wz_sql_am = 
            cfd_am %>% 
            dplyr::select(probe_timestamp, probe_tt, day_of_week, probe_hour, probe_minute)
        ## Prior Year
        # wz_sql_am_prior = func_sql_peak_hours(table_name=table_name_prior, hour_begin, hour_end, wz_start_prior, wz_end_prior, tmc_string, tmc_count)
        wz_sql_am_prior = 
            cfd_am %>% 
            dplyr::select(probe_timestamp, travel_time_50th, day_of_week, probe_hour, probe_minute)
        
        ## LOTTR
        # wz_sql_am_prior_lottr  = func_sql_peak_lottr(table_name=table_name_prior, hour_begin, hour_end, wz_start_lottr, wz_end_lottr, tmc_string, tmc_count)
        
        
        #####################
        # Midday Peak
        hour_begin = 10
        hour_end = 16
        
        # ## Current Year
        # wz_sql_mid = func_sql_peak_hours(table_name=table_name_wz, hour_begin, hour_end, wz_start, wz_end, tmc_string, tmc_count)
        # ## Prior Year
        # wz_sql_mid_prior = func_sql_peak_hours(table_name=table_name_prior, hour_begin, hour_end, wz_start_prior, wz_end_prior, tmc_string, tmc_count)
        
        ## Current Year
        wz_sql_mid = 
            cfd_mid %>% 
            dplyr::select(probe_timestamp, probe_tt, day_of_week, probe_hour, probe_minute)
        ## Prior Year
        wz_sql_mid_prior = 
            cfd_mid %>% 
            dplyr::select(probe_timestamp, travel_time_50th, day_of_week, probe_hour, probe_minute)
        
        ## LOTTR
        # wz_sql_mid_prior_lottr = func_sql_peak_lottr(table_name=table_name_prior,  hour_begin, hour_end, wz_start_lottr, wz_end_lottr, tmc_string, tmc_count)
        
        ########################
        # PM Peak
        hour_begin = 16
        hour_end = 20
        
        ## Current Year
        wz_sql_pm = 
            cfd_pm %>% 
            dplyr::select(probe_timestamp, probe_tt, day_of_week, probe_hour, probe_minute)
        ## Prior Year
        wz_sql_pm_prior = 
            cfd_pm %>% 
            dplyr::select(probe_timestamp, travel_time_50th, day_of_week, probe_hour, probe_minute)
        
        ## LOTTR
        # wz_sql_pm_prior_lottr = func_sql_peak_lottr(table_name=table_name_prior, hour_begin, hour_end, wz_start_lottr, wz_end_lottr, tmc_string, tmc_count)
        
        ########################
        # Weekend Peak
        hour_begin = 6
        hour_end = 20
        
        ## Current Year
        wz_sql_weekend = 
            cfd_weekend %>% 
            dplyr::select(probe_timestamp, probe_tt, day_of_week, probe_hour, probe_minute)
        ## Prior Year
        wz_sql_weekend_prior = 
            cfd_weekend %>% 
            dplyr::select(probe_timestamp, travel_time_50th, day_of_week, probe_hour, probe_minute)
        ## LOTTR
        # wz_sql_weekend_prior_lottr = func_sql_peak_lottr(table_name=table_name_prior, hour_begin, hour_end, wz_start_lottr, wz_end_lottr, tmc_string, tmc_count)
        
        
        func_create_cfd_plot(wz_id, OUTPUT_DIRECTORY, wz_sql_am, wz_sql_am_prior, cfd_case='tt_cfd_am') 
        func_create_cfd_plot(wz_id, OUTPUT_DIRECTORY, wz_sql_mid, wz_sql_mid_prior, cfd_case='tt_cfd_mid') 
        func_create_cfd_plot(wz_id, OUTPUT_DIRECTORY, wz_sql_pm, wz_sql_pm_prior, cfd_case='tt_cfd_pm') 
        func_create_cfd_plot(wz_id, OUTPUT_DIRECTORY, wz_sql_weekend, wz_sql_weekend_prior, cfd_case='tt_cfd_weekend') 
        ### END CFD Plots ###
        
        
        ### Start Eye Plots ###
        
        # Gray Prior WZ
        # Orange Current WZ
        
        # Eye Congestion
        wz_sql_eye = func_sql_eye(table_name_wz, tmc_string, wz_start, wz_end, wz_sql_distribution, 
                                  df_tmc_order, wz_id_value,
                                  wz_speed_limit=wz$speed[i], 
                                  threshold_speed_queue=threshold_speed_queue, 
                                  threshold_speed_congestion=threshold_speed_congestion)
        
        # wz_sql_eye$ss_congestion
        # list(ss_congestion, ss_delay, max_val_congestion, max_val_delay)
        
        g = func_plot_eye_chart(x=wz_sql_eye$eye_data_congestion, 
                                max_val=wz_sql_eye$max_val_congestion, 
                                case_eye_chart = 'congestion',
                                OUTPUT_DIRECTORY, wz_id)
        
        g = func_plot_eye_chart(x=wz_sql_eye$eye_data_delay, 
                                max_val=wz_sql_eye$max_val_delay, 
                                case_eye_chart = 'delay',
                                OUTPUT_DIRECTORY, wz_id)
        
        # wz_sql_eye_congestion = wz_sql_eye %>% 
        #     select(probe_hour, wz_hourly_congestion, wz_hourly_congestion_weekend)
        # wz_sql_eye_congestion_prior = wz_sql_eye %>% 
        #     select(probe_hour, hourly_typical_congestion, hourly_typical_congestion_weekend)
        # func_eye_chart_main(wz_id, OUTPUT_DIRECTORY, wz_sql_eye_congestion, wz_sql_eye_congestion_prior, case_eye_chart = 'congestion')
        # 
        # # Eye Delay
        # wz_sql_eye_delay = wz_sql_eye %>% 
        #     select(probe_hour, wz_hourly_delay)
        # wz_sql_eye_delay_prior = wz_sql_eye %>% 
        #     select(probe_hour, typical_hourly_delay)
        # 
        # func_eye_chart_main(wz_id, OUTPUT_DIRECTORY, wz_sql_eye_delay, wz_sql_eye_delay_prior, case_eye_chart = 'delay')
        
        ### END Eye Plots ###
        
        
        ### Volcano Charts ###
        wz_sql_volcano_congestion_prior_year = func_sql_volcano_congestion_prior_year(table_name_prior, 
                                                                                      tmc_string, 
                                                                                      wz_prior_year_start, 
                                                                                      wz_prior_year_end,
                                                                                      threshold_speed_congestion,
                                                                                      threshold_speed_queue)
        wz_sql_volcano_congestion_wz = func_sql_volcano_congestion_wz(table_name_wz,
                                                                      tmc_string, 
                                                                      wz_start, 
                                                                      wz_end,
                                                                      threshold_speed_congestion,
                                                                      threshold_speed_queue)
        
        
        ## Workzone duration volcano
        if (nrow(wz_sql_volcano_congestion_wz) > 0) {
            volcano_graph_data = func_create_volcano_graph_data(wz_id_value, wz_sql_volcano_congestion_wz, 
                                                                df_tmc_order, volcano_data_type = 'congestion')
            g = func_create_volcano_plot(volcano_graph_data, wz$start_mm[i], wz$end_mm[i], case_volcano_chart='wz')
        } else {
            g = ggplot()
        }
        ggsave(paste0(OUTPUT_DIRECTORY, wz_id, "/", wz_id,"_TT_volcano_congestion", ".png"), dpi=300, width=6, height=7)
        
        
        
        ## prior year volcano
        if (nrow(wz_sql_volcano_congestion_prior_year) > 0) {
            volcano_graph_data = func_create_volcano_graph_data(wz_id_value, wz_sql_volcano_congestion_prior_year, 
                                                                df_tmc_order, volcano_data_type = 'congestion')
            
            g = func_create_volcano_plot(volcano_graph_data, wz$start_mm[i], wz$end_mm[i], case_volcano_chart='prior')
            
        } else {
            g = ggplot()
        }
        ggsave(paste0(OUTPUT_DIRECTORY, wz_id, "/", wz_id,"_TT_year_prior_volcano_congestion", ".png"), dpi=300, width=6, height=7)
        
        
        wz_sql_max_queue_length = func_sql_max_queue_length(table_name_wz, tmc_string, wz_start, wz_end)
        
        summary = func_summarize(wz, i, 
                                 wz_sql_distribution, wz_sql_max_queue_length)
        
        wz_summary_table_write = summary[['wz_summary_table']] %>% as.data.table()
        
        end.time <- Sys.time()
        time.taken <- difftime(end.time, start.time, units='secs') %>% as.numeric()
        
        
        time_taken_indiv = c(time_taken_indiv, time.taken)
        
        
        wz_summary_table_write[, `:=` (time_taken = time.taken)]
        
        fwrite(x= wz_summary_table_write, file=paste0(OUTPUT_DIRECTORY, 'wz_results.csv'), append = TRUE)
        
        func_write_report(wz_id, summary, OUTPUT_DIRECTORY, path_ppt_file=path_ppt_file)
        
        print(paste0("It took ", round(sum(time.taken)/60, 1), " minutes to run the report for ", wz_id_value))
        
    }
    
    wz_results = fread(paste0(OUTPUT_DIRECTORY, 'wz_results.csv' ))
    wz_info = wz %>% 
        select(wz_id, county, road, category, direction, speed, wz_start, wz_end, start_mm, end_mm, tmc_count, wz_duration)
    
    wz_results$wz_id  = wz_results$wz_id %>% as.character()
    
    wz_results = left_join(wz_results, wz_info)
    fwrite(wz_results, file = paste0(OUTPUT_DIRECTORY, 'wz_results2.csv' ), append=TRUE)
    
    # wz_summary_table_combi = rbindlist(wz_summary_table, use.names=TRUE, fill=TRUE)
    # write.csv(wz_summary_table_combi, file=paste0(OUTPUT_DIRECTORY, 'wz_results.csv'), append = TRUE)
    
    
    # end.time <- Sys.time()
    # time.taken <- end.time - start.time
    # time.taken
    print("DONE!!!!")
    
}

############# END
# 
# 
# # ## length of all tmc segments chosen for work zone to calculate free flow travel time using speed limit
# mm = df_tmc_order
# len_alltmc = abs(max(mm$end_mm) - min(mm$start_mm))
# len_alltmc
# 
# ##### new summary statistic approach - delay and LOTTR metrics
# delay_new <- wz_sql_distribution
# delay_new = delay_new %>% dplyr::select(probe_hour,day_of_week,probe_tt,travel_time_50th, probe_timestamp)
# ####
# delay_new['delay'] = delay_new['probe_tt'] - delay_new['travel_time_50th']
# delay_new[,c('delay')] <- replace(delay_new[,c('delay')], delay_new[,c('delay')] <0,0)
# ### filter records for morning time period
# delay_am <- delay_new %>% filter(probe_hour %in% c('6','7','8','9') &
#                                    day_of_week %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
# ### filter records for mid day time period
# delay_mid <- delay_new %>% filter(probe_hour %in% c('10','11','12','13','14','15') &
#                                     day_of_week %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
# ### filter records for evening time period
# delay_pm <- delay_new %>% filter(probe_hour %in% c('16','17','18','19') &
#                                    day_of_week %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
# ### filter records for weekend time period
# delay_weekend <- delay_new %>% filter(probe_hour %in% c('6','7','8','9','10','11','12','13','14','15','16','17','18','19') &
#                                         day_of_week %in% c('Satureday', 'Sunday'))
# ### delay metrics in am period
# avg_am_delay <- median(delay_am$delay)
# max_am_delay <- max(delay_am$delay)
# total_am_delay <- sum(delay_am$delay)
# wz_am_lottr <- quantile(delay_am$probe_tt, probs = 0.85)[[1]]/quantile(delay_am$probe_tt, probs = 0.50)[[1]]
# ### delay metrics in mid period
# avg_mid_delay <- median(delay_mid$delay)
# max_mid_delay <- max(delay_mid$delay)
# total_mid_delay <- sum(delay_mid$delay)
# wz_mid_lottr <- quantile(delay_mid$probe_tt, probs = 0.85)[[1]]/quantile(delay_mid$probe_tt, probs = 0.50)[[1]]
# ### delay metrics in pm period
# avg_pm_delay <- median(delay_pm$delay)
# max_pmd_elay <- max(delay_pm$delay)
# total_pm_delay <- sum(delay_pm$delay)
# wz_pm_lottr <- quantile(delay_pm$probe_tt, probs = 0.85)[[1]]/quantile(delay_pm$probe_tt, probs = 0.50)[[1]]
# ### delay metrics in weekend period
# avg_weekend_delay <- median(delay_weekend$delay)
# max_weekend_delay <- max(delay_weekend$delay)
# total_weekend_delay <- sum(delay_weekend$delay)
# wz_weekend_lottr <- quantile(delay_weekend$probe_tt, probs = 0.85)[[1]]/quantile(delay_weekend$probe_tt, probs = 0.50)[[1]]
# ### Overal metrics
# avg_wz_delay <- median(delay_new$delay)
# max_wz_delay <- max(delay_new$delay)
# total_wz_delay <- sum(delay_new$delay)
# wz_lottr <- quantile(delay_new$probe_tt, probs = 0.85)[[1]]/quantile(delay_new$probe_tt, probs = 0.50)[[1]]
# ### ################################  queue length
# wz_queue <- as.data.frame(wz_sql_max_queue_length)
# max_queue_length <- max(wz_queue[,2])
# #num_queue <- nrow(wz_queue %>%
# #                    filter(wz_queue$queue_len > 0))
# #
# ################################  queue duration
# #total_queue_duration <- num_queue*5
# setDT(wz_queue)
# wz_queue <- wz_queue[order(wz_queue$min_Probe)]
# ####
# wz_queue[, queue_presence := +(wz_queue$queue_len > 0)
#          ][, queue_duration := with(rle(queue_presence), rep(lengths,lengths))
#            ][queue_presence == 0, queue_duration := 0]
# ####
# wz_queue[, queue_lag := +(wz_queue$queue_len == 0)
#          ][, queue_lag_duration := with(rle(queue_lag), rep(lengths,lengths))
#            ][queue_lag == 0, queue_lag_duration := 0]
# ####
# wz_queue <- wz_queue %>% mutate(queue_presence_modified =ifelse(wz_queue$queue_lag_duration == 1,1,wz_queue$queue_presence))
# ####
# setDT(wz_queue)
# wz_queue <- wz_queue[order(wz_queue$min_Probe)]
# ####
# wz_queue[, queue_presence_modified1 := +(wz_queue$queue_presence_modified == 1)
#          ][, queue_duration_modified := with(rle(queue_presence_modified1), rep(lengths,lengths))
#            ][queue_presence_modified1 == 0, queue_duration_modified := 0]
# ####
# wz_queue <- wz_queue %>% mutate(diff = queue_presence_modified1-lag(queue_presence_modified1))
# ###
# wz_queue_formation <- wz_queue %>% filter(wz_queue$diff ==1)
# num_queue <- nrow(wz_queue_formation)
# total_queue_duration <- nrow(wz_queue %>% filter(wz_queue$queue_presence_modified1 ==1))*5
# max_queue_duration <- max(wz_queue$queue_duration_modified)*5

# print("DONE!!!!")

