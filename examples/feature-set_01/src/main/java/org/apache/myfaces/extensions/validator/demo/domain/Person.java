/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.demo.domain;

import org.apache.myfaces.extensions.validator.crossval.annotation.DateIs;
import org.apache.myfaces.extensions.validator.crossval.annotation.DateIsType;
import org.apache.myfaces.extensions.validator.crossval.annotation.NotEquals;
import org.apache.myfaces.extensions.validator.baseval.annotation.Length;
import org.apache.myfaces.extensions.validator.baseval.annotation.Pattern;

import javax.persistence.Column;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import java.util.Date;

public class Person
{
    @Length(minimum = 2)
    @Column(nullable = false, length = 20)
    @NotEquals("lastName")
    @Pattern("[A-Z][a-z]+")
    private String firstName;

    @Length(minimum = 2)
    @Column(nullable = false, length = 20)
    private String lastName;

    @Column(nullable = false, length = 10)
    private String password;

    @Column(nullable = false, length = 10)
    private String role = "user";

    @DateIs(type = DateIsType.before, valueOf = "finalExam")
    @Column(nullable = false)
    @Temporal(TemporalType.DATE)
    private Date birthday;

    @Column(nullable = false)
    @Temporal(TemporalType.DATE)
    private Date finalExam;

    @Length(minimum = 2)
    @Column(nullable = false, length = 10)
    private String nickName;

    @Length(minimum = 6)
    @Column(nullable = false, length = 60)
    private String email;

    @Column(nullable = false)
    private int numberOfSiblings;

    public String getFirstName()
    {
        return firstName;
    }

    public void setFirstName(String firstName)
    {
        this.firstName = firstName;
    }

    public String getLastName()
    {
        return lastName;
    }

    public void setLastName(String lastName)
    {
        this.lastName = lastName;
    }

    public String getPassword()
    {
        return password;
    }

    public void setPassword(String password)
    {
        this.password = password;
    }

    public String getRole()
    {
        return role;
    }

    public void setRole(String role)
    {
        this.role = role;
    }

    public Date getBirthday()
    {
        return birthday;
    }

    public void setBirthday(Date birthday)
    {
        this.birthday = birthday;
    }

    public Date getFinalExam()
    {
        return finalExam;
    }

    public void setFinalExam(Date finalExam)
    {
        this.finalExam = finalExam;
    }

    public String getNickName()
    {
        return nickName;
    }

    public void setNickName(String nickName)
    {
        this.nickName = nickName;
    }

    public String getEmail()
    {
        return email;
    }

    public void setEmail(String email)
    {
        this.email = email;
    }

    public int getNumberOfSiblings()
    {
        return numberOfSiblings;
    }

    public void setNumberOfSiblings(int numberOfSiblings)
    {
        this.numberOfSiblings = numberOfSiblings;
    }
}
