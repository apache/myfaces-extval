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
package org.apache.myfaces.extensions.validator.core.metadata.transformer;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.Map;
import java.util.List;
import java.lang.annotation.Annotation;

/**
 * it's just a helper for proxies - you just need it, if you define the equivalent validation strategy as bean and
 * e.g. spring creates a proxy for it.
 *
 * if there is also a proxy for the transformer you can use the className property to manually repeat the
 * full qualified class name.
 *
 * @see org.apache.myfaces.extensions.validator.core.validation.strategy.BeanValidationStrategyAdapter
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.REUSE})
public class BeanMetaDataTransformerAdapterImpl extends AbstractMetaDataTransformer
    implements BeanMetaDataTransformerAdapter
{
    protected final Log logger = LogFactory.getLog(getClass());

    private MetaDataTransformer metaDataTransformer;
    private String metaDataTransformerClassName;

    public Map<String, Object> processExtraction(Annotation annotation)
    {
        if(this.metaDataTransformer instanceof AbstractMetaDataTransformer)
        {
            return ((AbstractMetaDataTransformer)this.metaDataTransformer).processExtraction(annotation);
        }
        return this.metaDataTransformer.extractMetaData(annotation);
    }

    @Override
    protected String getSkipExpression(Annotation annotation)
    {
        if(this.metaDataTransformer instanceof AbstractMetaDataTransformer)
        {
            return ((AbstractMetaDataTransformer)this.metaDataTransformer).getSkipExpression(annotation);
        }
        return super.getSkipExpression(annotation);
    }

    @Override
    protected List<String> getMetaDataKeys()
    {
        if(this.metaDataTransformer instanceof AbstractMetaDataTransformer)
        {
            return ((AbstractMetaDataTransformer)this.metaDataTransformer).getMetaDataKeys();
        }
        return super.getMetaDataKeys();
    }

    public String getMetaDataTransformerClassName()
    {
        if(metaDataTransformerClassName != null)
        {
            return metaDataTransformerClassName;
        }
        if(metaDataTransformer.getClass().getPackage() != null)
        {
            metaDataTransformer.getClass();
        }

        return null;
    }

    /*
     * generated
     */
    public void setMetaDataTransformerClassName(String metaDataTransformerClassName)
    {
        this.metaDataTransformerClassName = metaDataTransformerClassName;
    }

    public MetaDataTransformer getMetaDataTransformer()
    {
        return metaDataTransformer;
    }

    public void setMetaDataTransformer(MetaDataTransformer metaDataTransformer)
    {
        this.metaDataTransformer = metaDataTransformer;
    }
}