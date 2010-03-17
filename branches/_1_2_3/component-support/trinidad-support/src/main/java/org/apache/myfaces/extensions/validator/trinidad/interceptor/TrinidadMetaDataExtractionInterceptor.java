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

import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DisableClientSideValidation;
import org.apache.myfaces.extensions.validator.core.interceptor
        .ComponentInitializationAwareMetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@InvocationOrder(300)
@UsageInformation(UsageCategory.INTERNAL)
public class TrinidadMetaDataExtractionInterceptor extends ComponentInitializationAwareMetaDataExtractionInterceptor
{
    protected void afterExtractingForComponentInitialization(PropertyInformation propertyInformation)
    {
        processMetaDataEntries(propertyInformation.getMetaDataEntries());
    }

    private void processMetaDataEntries(MetaDataEntry[] metaDataEntries)
    {
        for (MetaDataEntry entry : metaDataEntries)
        {
            if (processEntry(entry) && isClientValidationDisabled(entry))
            {
                disableClientSideValidation(entry);
            }
        }
    }

    //e.g. returns false for jsr303 entries
    private boolean processEntry(MetaDataEntry entry)
    {
        return entry.getValue() instanceof Annotation;
    }

    private boolean isClientValidationDisabled(MetaDataEntry entry)
    {
        return ExtValUtils.getValidationParameterExtractor()
                .extract(entry.getValue(Annotation.class),
                        ExtValUtils.getValidationParameterClassFor(DisableClientSideValidation.class))
                .iterator().hasNext();
    }

    @SuppressWarnings({"unchecked"})
    private void disableClientSideValidation(MetaDataEntry entry)
    {
        if(entry.getProperty(CommonMetaDataKeys.DISABLE_CLIENT_SIDE_VALIDATION) == null)
        {
            entry.setProperty(CommonMetaDataKeys.DISABLE_CLIENT_SIDE_VALIDATION, new ArrayList<String>());
        }

        List<String> keysToDisable = entry.getProperty(CommonMetaDataKeys.DISABLE_CLIENT_SIDE_VALIDATION, List.class);
        keysToDisable.add(entry.getKey());
    }
}
