/*
 * sick_drv.h
 *
 *  Created on: 2011-05-28
 *      Author: Damian T. Dobroczy\\'nski <qoocku@gmail.com>
 *      Author: Dariusz Pazderski <dariusz.pazderski@put.poznan.pl>
 */

#ifndef SICK_DRV_H_
#define SICK_DRV_H_

#define SICK_MAX_FRAMES 20

struct distance_frame
{
  unsigned short int distance[362];
  int time;
  unsigned char status;
};
typedef struct distance_frame distance_frame_t;

typedef void* sick_handle_t;

sick_handle_t
sick_open();

int
sick_configure(sick_handle_t);

int
sick_start(sick_handle_t);

int
sick_read_stream(sick_handle_t);

void
sick_clear_buffer(sick_handle_t);

unsigned short int *
sick_get_current_scan(sick_handle_t);

void
sick_close(sick_handle_t);

#endif /* SICK_DRV_H_ */
