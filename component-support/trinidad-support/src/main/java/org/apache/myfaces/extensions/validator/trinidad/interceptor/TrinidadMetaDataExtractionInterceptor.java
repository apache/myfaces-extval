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
package org.apache.myfaces.extensions.validator.trinidad.interceptor;

import org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DisableClientSideValidation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import java.lang.annotation.Annotation;
import java.util.List;
import java.util.ArrayList;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
public class TrinidadMetaDataExtractionInterceptor implements MetaDataExtractionInterceptor
{
    public void afterExtracting(PropertyInformation propertyInformation)
    {
        List<MetaDataEntry> result = new ArrayList<MetaDataEntry>();

        for(MetaDataEntry entry : propertyInformation.getMetaDataEntries())
        {
            if(!(entry.getValue() instanceof Annotation &&
                    ExtValUtils.getValidationParameterExtractor()
                            .extract(entry.getValue(Annotation.class), DisableClientSideValidation.class)
                            .iterator().hasNext()))
            {
                result.add(entry);
            }
        }

        if(propertyInformation.getMetaDataEntries().length != result.size())
        {
            propertyInformation.resetMetaDataEntries();

            for(MetaDataEntry entry : result)
            {
                propertyInformation.addMetaDataEntry(entry);
            }
        }
    }
}
