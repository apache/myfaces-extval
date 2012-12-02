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
package org.apache.myfaces.extensions.validator.crossval.storage;

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.crossval.strategy.CrossValidationStrategy;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.component.UIComponent;

/**
 * normally it should be in the storage package - due to backward compatibility it isn't the case
 * 
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public class CrossValidationStorageEntry
{
    private MetaDataEntry metaDataEntry;
    //for complex components (e.g. a table) stores the object of entry (#{entry.property})
    private UIComponent component;
    private Object convertedObject;
    private CrossValidationStrategy validationStrategy;
    //just for input components within complex components e.g. dataTable,...
    private String clientId;

    public MetaDataEntry getMetaDataEntry()
    {
        return metaDataEntry;
    }

    public void setMetaDataEntry(MetaDataEntry metaDataEntry)
    {
        this.metaDataEntry = metaDataEntry;
    }

    public UIComponent getComponent()
    {
        return component;
    }

    public void setComponent(UIComponent component)
    {
        this.component = component;
    }

    public Object getConvertedObject()
    {
        return convertedObject;
    }

    public void setConvertedObject(Object convertedObject)
    {
        this.convertedObject = convertedObject;
    }

    public CrossValidationStrategy getValidationStrategy()
    {
        return validationStrategy;
    }

    public void setValidationStrategy(CrossValidationStrategy validationStrategy)
    {
        this.validationStrategy = validationStrategy;
    }

    public String getClientId()
    {
        return clientId;
    }

    public void setClientId(String clientId)
    {
        this.clientId = clientId;
    }
}
