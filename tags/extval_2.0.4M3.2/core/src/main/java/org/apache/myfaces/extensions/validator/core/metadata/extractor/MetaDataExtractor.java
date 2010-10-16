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
package org.apache.myfaces.extensions.validator.core.metadata.extractor;

import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import javax.faces.context.FacesContext;

/**
 * A meta-data extractor is responsible to analyze an object (UIComponent)
 * and returns all available meta-data information.
 * 
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface MetaDataExtractor
{
    /**
     * Returns all information about the object. Important remark: The default implementation,
     * DefaultComponentMetaDataExtractor, uses a MetaDataStorageFilter that can decide to not include some meta data.
     *
     * @param facesContext The JSF Faces Context
     * @param object The UIComponent from which we need all available information.
     * @return  All information related to the UIComponent.
     */
    PropertyInformation extract(FacesContext facesContext, Object object);
}
