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
package org.apache.myfaces.extensions.validator.test.propval;

import org.apache.myfaces.extensions.validator.baseval.annotation.DoubleRange;
import org.apache.myfaces.extensions.validator.baseval.annotation.Length;
import org.apache.myfaces.extensions.validator.baseval.annotation.Pattern;
import org.apache.myfaces.extensions.validator.baseval.annotation.Required;

/**
 */
public class BaseValTestBean
{
    @Required
    private String name;

    @Length(minimum=2,maximum=3)
    private String name1;

    @Pattern("[A-Z][a-z]+")
    private String patternName;

    @DoubleRange(minimum=-300d)
    private Double doubleValue1;

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public String getName1()
    {
        return name1;
    }

    public void setName1(String name1)
    {
        this.name1 = name1;
    }

    public String getPatternName()
    {
        return patternName;
    }

    public void setPatternName(String patternName)
    {
        this.patternName = patternName;
    }

    public Double getDoubleValue1()
    {
        return doubleValue1;
    }

    public void setDoubleValue1(Double doubleValue1)
    {
        this.doubleValue1 = doubleValue1;
    }
}