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
package org.apache.myfaces.extensions.validator.baseval.metadata.transformer;

import org.apache.myfaces.extensions.validator.baseval.annotation.JoinValidation;
import org.apache.myfaces.extensions.validator.baseval.annotation.extractor.DefaultPropertyScanningMetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DisableClientSideValidation;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.PropertyValidationUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.context.FacesContext;
import java.util.HashMap;
import java.util.Map;
import java.util.Collections;
import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
@Deprecated
public class JoinMetaDataTransformer implements MetaDataTransformer
{
    protected final Log logger = LogFactory.getLog(getClass());

    public Map<String, Object> convertMetaData(MetaDataEntry metaDataEntry)
    {
        try
        {
            return convert(metaDataEntry);
        }
        catch (Throwable t)
        {
            if(this.logger.isWarnEnabled())
            {
                this.logger.warn("this class is replaced by a meta-data storage filter. " +
                        "if it gets invoked and an exception occurs, a custom syntax is used." +
                        "this class might be used by an old add-on. please check for a newer version.");
            }
            return Collections.emptyMap();
        }
    }

    private Map<String, Object> convert(MetaDataEntry metaDataEntry)
    {
        MetaDataExtractor extractor = DefaultPropertyScanningMetaDataExtractor.getInstance();

        String[] targetExpressions = metaDataEntry.getValue(JoinValidation.class).value();

        ValidationStrategy validationStrategy;
        MetaDataTransformer metaDataTransformer;

        Map<String, Object> results = new HashMap<String, Object>();

        PropertyDetails propertyDetails;
        for (String targetExpression : targetExpressions)
        {
            propertyDetails = ExtValUtils
                .createPropertyDetailsForNewTarget(metaDataEntry, targetExpression);

            for (MetaDataEntry entry : extractor.extract(FacesContext.getCurrentInstance(),
                                                            propertyDetails).getMetaDataEntries())
            {
                validationStrategy = ExtValUtils.getValidationStrategyForMetaData(entry.getKey());

                if(validationStrategy == null ||
                        PropertyValidationUtils.isValidationSkipped(FacesContext.getCurrentInstance(),
                                validationStrategy, entry))
                {
                    continue;
                }

                metaDataTransformer = ExtValUtils.getMetaDataTransformerForValidationStrategy(validationStrategy);

                if (metaDataTransformer != null)
                {
                    if(!(entry.getValue() instanceof Annotation &&
                            ExtValUtils.getValidationParameterExtractor()
                                    .extract(entry.getValue(Annotation.class),
                                            ExtValUtils
                                                    .getValidationParameterClassFor(DisableClientSideValidation.class))
                                    .iterator().hasNext()))
                    {
                        results.putAll(metaDataTransformer.convertMetaData(entry));
                    }
                }
            }
        }
        return results;
    }
}
