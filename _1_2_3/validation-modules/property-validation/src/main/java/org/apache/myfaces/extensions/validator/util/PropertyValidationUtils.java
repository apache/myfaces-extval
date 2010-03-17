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
package org.apache.myfaces.extensions.validator.util;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.baseval.strategy.SkipValidationStrategy;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.context.FacesContext;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class PropertyValidationUtils
{
    private static final Log LOGGER = LogFactory.getLog(PropertyValidationUtils.class);

    public static boolean isValidationSkipped(FacesContext facesContext,
                                         ValidationStrategy validationStrategy,
                                         MetaDataEntry metaDataEntry)
    {
        if(ExtValUtils.isSkipableValidationStrategy(
                ProxyUtils.getUnproxiedClass(validationStrategy.getClass(), ValidationStrategy.class)))
        {
            Boolean skipValidation = metaDataEntry.getProperty(
                PropertyInformationKeys.SKIP_VALIDATION, Boolean.class);

            if(Boolean.TRUE.equals(skipValidation))
            {
                if(LOGGER.isTraceEnabled())
                {
                    LOGGER.trace("validation of " + validationStrategy.getClass().getName() + " canceled");
                }

                return true;
            }
        }
        else if(validationStrategy instanceof SkipValidationStrategy)
        {
            validationStrategy.validate(facesContext, null, metaDataEntry, null);
            return true;
        }
        return false;
    }
}
