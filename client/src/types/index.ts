export interface Buffer {
  id:   number;
  hash: string;
  text: string;
}

export interface DivBuffer { 
  div: string;
  buf: Buffer;
}

export interface Student { 
  grpBuffer: Buffer;
}

export interface Instructor {
  allBuffers: Array<Buffer>;
} 

export interface UserData {
  user: User;
  instructor: Array<Instructor>;
  student: Array<Student>;
}

export interface AuthInfo {
  emailAddress: string;
  password: string;
}

export interface User {
  firstName: string;
  lastName: string;
  group: string;
}

export interface LoginResponse {
  accessToken: string;
  user: UserData;
}