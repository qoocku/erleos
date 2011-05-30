/*
 * sick_drv.c
 *
 *  Created on: 2011-05-28
 *      Author: Damian T. Dobroczy\\'nski <qoocku@gmail.com>
 */

#include <math.h>
#include <stdio.h>   /* Standard input/output definitions */
#include <stdlib.h>
#include <string.h>  /* String function definitions */
#include <fcntl.h>   /* File control definitions */
#include <errno.h>   /* Error number definitions */
#include <sys/ioctl.h>
#include <sys/types.h>
#include <time.h>

#include "ftd2xx.h"

#include "sick_drv.h"

struct sick_drv
{
  distance_frame_t measurement[SICK_MAX_FRAMES];
  unsigned char temp_buffer[10000];
  unsigned short int scan[362];

  unsigned char * sick_cmd;
  unsigned char * sick_ans_header;

  int sick_cmd_size;
  int sick_ans_size;
  int frame_errors;

  struct timeval start_time;

  unsigned int index_frame_w, index_frame_r;

  FT_STATUS ftStatus;
  FT_HANDLE ftHandle;

};
typedef struct sick_drv sick_drv_t;

#define CRC16_GEN_POL  0x8005
#define MKSHORT(a,b)   ((unsigned short) (a) | ((unsigned short)(b) << 8))

#define SICK_ACK_CHAR   0x06

// wzorce komend do skanera
unsigned char SICK_CMD_500k[] = { 0x02, 0x00, 0x02, 0x00, 0x20, 0x48, 0x58, 0x08};
unsigned char SICK_CMD_STAT[] = { 0x02, 0x00, 0x01, 0x00, 0x31, 0x15, 0x12};
unsigned char SICK_CMD_STOP[] = { 0x02, 0x00, 0x02, 0x00, 0x20, 0x25, 0x35, 0x08};
unsigned char SICK_CMD_START[] = { 0x02, 0x00, 0x02, 0x00, 0x20, 0x24, 0x34, 0x08};

// wzorce odpowiedzi od skanera
unsigned char SICK_ANS_STAT[] = { 0x02, 0x80, 0x9A, 0x00, 0xB1};
unsigned char SICK_ANS_500k[] = { 0x02, 0x80, 0x03, 0x00, 0xA0};
unsigned char SICK_ANS_SCAN[] = { 0x02, 0x80, 0xD6, 0x02, 0xB0};


// definicja rozmiarow ramek zapytania
#define SICK_CMD_STAT_SIZE  7
#define SICK_CMD_500k_SIZE  8
#define SICK_CMD_START_SIZE  8

// definicja rozmiarow ramek odpowiedzi
#define SICK_ANS_STAT_SIZE  160
#define SICK_ANS_500k_SIZE  9
#define SICK_ANS_SCAN_SIZE  732

// liczba maksymalnych blednych ramek w ciagu
#define SICK_FRAME_ERRORS_LIMIT 5

int
send_command (sick_drv_t*);

int
send_commands (sick_drv_t*, int n);

int
port_set_speed (sick_drv_t*);

int
status (sick_drv_t*);

int
port_set_low_speed (sick_drv_t*);

int
port_set_hi_speed (sick_drv_t*);

int
port_init (sick_drv_t*);

int
data_read (sick_drv_t*, int, int);

int
detect_header (sick_drv_t*, unsigned char, int);

int
find_header (sick_drv_t*, unsigned char*, int, int);

int
go_high_speed (sick_drv_t*);

unsigned short
update_crc_sick (sick_drv_t*, unsigned short, char, char);

int
check_frame (sick_drv_t*, unsigned char*, int);

//--------------------------------------------------------------------------------------------------------------
// realizacja wyslania komendy do urzadzenia z potwierdzeniem
//--------------------------------------------------------------------------------------------------------------
int
send_command (sick_drv_t* self)
{
  int i;
  DWORD l;

  self->frame_errors = 0;
  sick_clear_buffer(self); // flush receive buffer
  self->ftStatus = FT_Write(self->ftHandle,
                              self->sick_cmd,
                              self->sick_cmd_size, &l); // send command
  i = data_read(self, self->sick_ans_size + 1, 0); // data read (with timeout)
  if (i == 1)
    {
      i = find_header(self, self->sick_ans_header,
                        self->sick_ans_size + 1, 5); // search for frame header
      if (i >= 0)
        {
          if (self->temp_buffer[i - 1] == SICK_ACK_CHAR)
            {
              if (check_frame(self, &self->temp_buffer[i], self->sick_ans_size)
                  < 0)
                {
                  return -1;
                }
              return 0;
            }
          else
            return -1;
        }
      else
        return -1;
    }
  else
    {
      return self->ftStatus;
    }
}

//--------------------------------------------------------------------------------------------------------------
// realizacja wyslania komendy do urzadzenia z powtorzeniami
// we: n - liczba powtorzen w przypadku braku komunikacji
//--------------------------------------------------------------------------------------------------------------
int
send_commands(sick_drv_t* self, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      if (send_command(self) == 0)
        {
          return 0; // wyslano poprawnie
        }
    }
  return -1; // blad
}

//--------------------------------------------------------------------------------------------------------------
// Otwarcie portu: res = 0: OK, res = -1: error
//--------------------------------------------------------------------------------------------------------------
int
port_init (sick_drv_t* self)
{
  // open communication port
  self->ftStatus = FT_Open(0, &self->ftHandle);
  if (!FT_SUCCESS(self->ftStatus))
    {
      return self->ftStatus;
    }
  if (FT_SUCCESS(self->ftStatus = FT_SetTimeouts(self->ftHandle, 2000, 2000))) //TODO: change constatnts to parameters
    return 0;
  else
    return -1;
}

//--------------------------------------------------------------------------------------------------------------
// Funkcja odczytu bufora danych odebranych
// we: size - liczba znakow do odczytu
//--------------------------------------------------------------------------------------------------------------
int
data_read(sick_drv_t* self, int size, int offset)
{
  int i;
  DWORD len;

  i = 0;

  self->ftStatus = FT_Read(self->ftHandle,
                            &self->temp_buffer[offset], size,
                            &len);
  if (FT_SUCCESS(self->ftStatus))
    {
      if (len == size)
        {
          return 0; // proper data
        }
      else
        {
          // FT_Read Timeout
          return -1;
        }
    }
  else
    {
      return -1;
      // FT_Read Failed
    }
}

//--------------------------------------------------------------------------------------------------------------
// Wyslanie komendy przelaczenia predkosci na 500kbs
//--------------------------------------------------------------------------------------------------------------

int
go_high_speed(sick_drv_t* self)
{
  self->sick_cmd        = SICK_CMD_500k;
  self->sick_ans_header = SICK_ANS_500k;
  self->sick_cmd_size   = SICK_CMD_500k_SIZE;
  self->sick_ans_size   = SICK_ANS_500k_SIZE;

  return send_commands(self, 2);
}

//--------------------------------------------------------------------------------------------------------------
// Ustawienie predkosci portu na 9600 bps
//--------------------------------------------------------------------------------------------------------------

int
port_set_low_speed (sick_drv_t* self)
{
  if (!FT_SUCCESS(self->ftStatus = FT_SetBaudRate(self->ftHandle, 9600)))
    {
      return -1;
    }
  return 0; // OK
}

//--------------------------------------------------------------------------------------------------------------
// Ustawienie predkosci portu na 500 kbps
//--------------------------------------------------------------------------------------------------------------

int
port_set_hi_speed(sick_drv_t* self)
{
  if (!FT_SUCCESS(self->ftStatus = FT_SetBaudRate(self->ftHandle, 500000)))
    {
      return -1;
    }
  return 0; // OK
}

//--------------------------------------------------------------------------------------------------------------
// Czyszczenie bufora urzadzenia wejciowego FTDI
//--------------------------------------------------------------------------------------------------------------
void
sick_clear_buffer(sick_handle_t h)
{
  sick_drv_t* self = (sick_drv_t*)h;
  self->ftStatus = FT_Purge(self->ftHandle, FT_PURGE_RX);
}


sick_handle_t
sick_open ()
{
  sick_drv_t* self = malloc(sizeof(sick_drv_t));
  bzero(self->measurement, 362*sizeof(distance_frame_t));
  return self;
}

void
sick_close (sick_handle_t h)
{
  FT_Close(((sick_drv_t*)h)->ftHandle);
  free((sick_drv_t*)h);
}

//--------------------------------------------------------------------------------------------------------------
// Zapytanie o status urzadzenia
//--------------------------------------------------------------------------------------------------------------

int
sick_status (sick_handle_t h)
{
  sick_drv_t* self = (sick_drv_t*)h;

  self->sick_cmd        = SICK_CMD_STAT;
  self->sick_ans_header = SICK_ANS_STAT;
  self->sick_cmd_size   = SICK_CMD_STAT_SIZE;
  self->sick_ans_size   = SICK_ANS_STAT_SIZE;

  return send_commands(self, 2);
}

//--------------------------------------------------------------------------------------------------------------
// Podstawowa konfiguracja Sicka
//--------------------------------------------------------------------------------------------------------------

int
sick_configure(sick_handle_t h)
{
  sick_drv_t* self = (sick_drv_t*) h;
  if (port_init(self) == 0)
    {
      if (port_set_low_speed(self) == 0)
        {
          // pytamy o status urzadzenia na 9600bps (domyslnie po zalaczeniu Sicka)
          if (sick_status(h) != 0)
            {
              if (port_set_hi_speed(self) == 0)
              {
                if (sick_status(h) != 0)
                return -1;
                else
                  {
                    if (go_high_speed(self) != 0) // przelaczenie Sicka na 500kbps
                      return port_set_hi_speed(self); // przelaczenie portu na 500kbps
                    else
                      return 0;
                  }
              }
            else
              return -1;
            }
          else
            return -1;
        }
      else
        return -1;
    }
  else
    return -1;
}

//--------------------------------------------------------------------------------------------------------------
// Rozpoczecie pomiaru ciaglego
//--------------------------------------------------------------------------------------------------------------
int
sick_start(sick_handle_t h)
{
  sick_drv_t* self = (sick_drv_t*) h;
  // zerowanie wskaznikow bufora cyklicznego
  self->index_frame_r = self->index_frame_w = 0;
  // zapisanie czasu poczatkowego
  gettimeofday(&self->start_time, NULL);

  self->sick_cmd        = SICK_CMD_START;
  self->sick_ans_header = SICK_ANS_STAT;
  self->sick_cmd_size   = SICK_CMD_START_SIZE;
  self->sick_ans_size   = SICK_ANS_STAT_SIZE;

  return send_command(self);
}

//--------------------------------------------------------------------------------------------------------------
// poszukiwanie naglowka wiadomosci
// wyjscie: przesuniecie wzgledem poczatku bufora gdzie jest poczatek naglowka
//--------------------------------------------------------------------------------------------------------------

int
find_header(sick_drv_t* self,
              unsigned char * head_pattern,
              int data_size,
              int header_length)
{
  int i, j;
  for (i = 0; i < data_size - header_length; i++)
    {
      for (j = 0; j < header_length; j++)
        {
          if (self->temp_buffer[i + j] != head_pattern[j])
            break;
        }
      if (j == header_length)
        return i; // znaleziono naglowek
    }
  return -1; //nie znaleziono naglowka
}

//--------------------------------------------------------------------------------------------------------------
// czytanie ramki danych Sicka w trybie ciaglym
// wpisanie ramki do
//--------------------------------------------------------------------------------------------------------------

int
sick_read_stream(sick_handle_t h)
{
  sick_drv_t* self = (sick_drv_t*)h;
  int i;
  int time;
  struct timeval current_time;

  self->sick_ans_header = SICK_ANS_SCAN;
  self->sick_ans_size = SICK_ANS_SCAN_SIZE;

  // check how many frames are lost
  if (self->frame_errors > SICK_FRAME_ERRORS_LIMIT)
    {
      sick_clear_buffer(self);
    }

  // try to get one full frame
  i = data_read(self, self->sick_ans_size, 0); // data read (with timeout)
  if (i == 0)
    {
      // now search for the frame header
      i = find_header(self, self->sick_ans_header, self->sick_ans_size, 5);
      // check if you got full frame
      if (i > 0)
        {
          // no: some data have not been read yet (as a result of some missing characters in the buffer)
          // they must be read to complete the frame
          self->frame_errors++;
          return -1; // some error occured
        }
      else
        {
          // the frame has been completed
          if (check_frame(self, &self->temp_buffer[i], self->sick_ans_size) != 0)
            {
              self->frame_errors++;
              return -1;
            }
        }
    }
  else
    {
      self->frame_errors++;
      return self->ftStatus;
    }
  self->frame_errors = 0; // ramka prawidlowa

  // przepisanie do bufora cyklicznego odleglosci
  memcpy(self->measurement[self->index_frame_w].distance,
          &self->temp_buffer[i + 5], 724);
  // przepisanie statusu
  self->measurement[self->index_frame_w].status = self->temp_buffer[i + 729];
  // zarejestrowanie czasu
  gettimeofday(&current_time, NULL);
  time = current_time.tv_sec - self->start_time.tv_sec;
  time = time * 1000 + (current_time.tv_usec - self->start_time.tv_usec) / 1000;

  self->measurement[self->index_frame_w].time = time;
  self->index_frame_w++;
  self->index_frame_w %= SICK_MAX_FRAMES; // zapetlenie bufora cyklicznego

  return 0;
}

unsigned short int *
sick_get_current_scan(sick_handle_t h)
{
  sick_drv_t* self = (sick_drv_t*) h;
  unsigned int i;
  i = (self->index_frame_w - 1) % SICK_MAX_FRAMES;
  memcpy(self->scan, self->measurement[self->index_frame_w].distance, sizeof(unsigned short int));
  return self->scan;
}

//--------------------------------------------------------------------------------------------------------------
// Funkcja sprawdzenia ramki
//--------------------------------------------------------------------------------------------------------------

int
check_frame(sick_drv_t* self, unsigned char * frame, int frame_length)
{
  unsigned short uCrc16 = 0, crc;
  unsigned char abData[] = { 0, 0 };
  unsigned char * data = frame;
  int uLen = frame_length - 2;
  while (uLen--)
    {
      abData[1] = abData[0];
      abData[0] = *data++;
      if (uCrc16 & 0x8000)
        {
          uCrc16 = (uCrc16 & 0x7fff) << 1;
          uCrc16 ^= CRC16_GEN_POL;
        }
      else
        {
          uCrc16 <<= 1;
        }
      uCrc16 ^= MKSHORT (abData[0] , abData[1]);
    }
  // czytaj crc z ramki
  crc = MKSHORT (frame[frame_length-2] , frame[frame_length-1]);
  if (crc != uCrc16)
    return -1; // blad
  return 0; // OK
}
