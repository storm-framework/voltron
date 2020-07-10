export interface Buffer {
  id:   string;
  hash: string;
  text: string;
  div:  string;
}

export interface Student { 
  tag: "Student";
  class: string;
  grpBuffer: Buffer;
}

export interface Instructor {
  tag: "Instructor";
  class: string;
  allBuffers: Buffer[];
} 

export interface UserData {
  user: User;
  classes: ClassData[];
}

export type ClassData = Instructor | Student;

export interface AuthInfo {
  emailAddress: string;
  password: string;
}

export interface User {
  firstName: string;
  lastName: string;
}

export interface LoginResponse {
  accessToken: string;
  user: User;
}

export interface ClassView<T> {
  name: string;
  index: number;
  data: T;
}

export interface Enroll {
  className: string;
  buffers: Buffer[];
  enrolls: EnrollInfo[];
}

export interface EnrollInfo {
  email: string;
  firstName: string;
  lastName: string;
  group: string;
};