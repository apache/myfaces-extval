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
package org.apache.myfaces.extensions.validator.crossval;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import javax.faces.component.UIComponent;
import java.util.List;

/**
 * In order to build up a mapping which is used for cross-validation.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ProcessedInformationEntry
{
    private Object bean;
    private Object convertedValue;
    private UIComponent component;
    //for complex components (e.g. a table there are multiple entries with
    //the same key (here the el expression #{entry.property})
    //however, don't override the previous entry - they arn't the same;
    private List<ProcessedInformationEntry> furtherEntries;

    /*
     * generated
     */
    public Object getBean()
    {
        return bean;
    }

    public void setBean(Object bean)
    {
        this.bean = bean;
    }

    public Object getConvertedValue()
    {
        return convertedValue;
    }

    public void setConvertedValue(Object convertedValue)
    {
        this.convertedValue = convertedValue;
    }

    public UIComponent getComponent()
    {
        return component;
    }

    public void setComponent(UIComponent component)
    {
        this.component = component;
    }

    public List<ProcessedInformationEntry> getFurtherEntries()
    {
        return furtherEntries;
    }

    public void setFurtherEntries(List<ProcessedInformationEntry> furtherEntries)
    {
        this.furtherEntries = furtherEntries;
    }
}
